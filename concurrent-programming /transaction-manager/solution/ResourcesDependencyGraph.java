package cp1.solution;

import cp1.base.ResourceId;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;

/**
 * Class for graph describing dependencies of transactions stopped while
 * awaiting for certain resource. The graph is used for detecting deadlocks.
 */
public class ResourcesDependencyGraph {
    public static class ResourceNode {
        public volatile boolean aborted;
        public volatile int index;
        public volatile long owner;
        public volatile long ownerTransactionStart;
        public ResourceId rid;

        public ResourceNode(ResourceId rid, int i) {
            aborted = false;
            index = i;
            owner = 0L;
            ownerTransactionStart = 0;
            this.rid = rid;
        }
    }

    private final ResourceNode[] nodes;
    // 'edges[i] = j' means transaction owning resource 'i' waits for resource 'j'.
    private final Integer[] edges;
    private Semaphore mutex;
    private ForceMutexAcquire forceAcquirer;

    public ResourcesDependencyGraph(ResourcesManager resources) {
        int resourcesLen = resources.getResourcesInfoLen();
        nodes = new ResourceNode[resourcesLen];
        edges = new Integer[resourcesLen];

        resources.setResourceDependencyGraph(nodes, edges);

        mutex = new Semaphore(1, true);
        forceAcquirer = new ForceMutexAcquire(mutex);
    }

    public void updateNodeInfo(ResourceId rid, boolean aborted, long owner, long ownerTransactionStart) {
        ResourceNode updatedNode = getNode(rid);

        updatedNode.aborted = aborted;
        updatedNode.owner = owner;
        updatedNode.ownerTransactionStart = ownerTransactionStart;
    }

    /**
     * Deletes edges indicating that current thread is no longer awaiting for
     * for any resource.
     * @param ownedResources list of resources owned by current thread
     */
    public void updateEdges(ArrayList<ResourceId> ownedResources) {
        forceAcquirer.acquire();

        for (ResourceId rid : ownedResources) {
            edges[getNode(rid).index] = null;
        }

        mutex.release();
    }

    /**
     * Add information about transactions that is awaiting for the resource.
     * @param transaction awaiting transaction
     * @param wantedResource resource for which it is awaiting
     */
    public void addTransaction(TransactionInfo transaction, ResourceId wantedResource) {
        forceAcquirer.acquire();

        ResourceNode ownedResourceNode;
        ResourceNode wantedNode = getNode(wantedResource);
        for (ResourceId rid : transaction.ownedResources) {
            ownedResourceNode = getNode(rid);
            edges[ownedResourceNode.index] = wantedNode.index;
        }

        mutex.release();
    }

    public ResourceNode getNode(ResourceId rid) {
        for (ResourceNode node : nodes) {
            if (node.rid == rid) {
                return node;
            }
        }

        return null;
    }

    /**
     * Aborts transaction in cycle.
     * @param cycle cycle of awaiting resources generating deadlock
     * @param transactionsInfo information about all current transactions
     */
    private void abortTransaction(ArrayList<ResourceNode> cycle,
                                 ConcurrentHashMap<Long, TransactionInfo> transactionsInfo) {
        long latestStartingTime = cycle.get(0).ownerTransactionStart;
        long threadIdToAbort = cycle.get(0).owner;

        long tmpTime;
        long tmpThreadId;
        for (ResourceNode node : cycle) {
            tmpTime = node.ownerTransactionStart;
            tmpThreadId = node.owner;

            if (latestStartingTime < tmpTime || (latestStartingTime == tmpTime && threadIdToAbort < tmpThreadId)) {
                latestStartingTime = tmpTime;
                threadIdToAbort = tmpThreadId;
            }
        }

        TransactionInfo tInfo = transactionsInfo.get(threadIdToAbort);
        tInfo.abortTransaction();

        for (ResourceNode node : nodes) {
            if (node.owner == threadIdToAbort) {
                node.aborted = true;
            }
        }

        tInfo.thread.interrupt();
    }

    /**
     * Checks if 'startingRid' is in cycle which indicates that deadlock
     * occurred. If yes, one of the transactions in cycle gets aborted,
     * otherwise nothing happens.
     * @param startingRid ResourceId of starting node
     * @param transactionsInfo information about all current transactions
     */
    public void checkDeadlock(ResourceId startingRid, ConcurrentHashMap<Long, TransactionInfo> transactionsInfo) {
        forceAcquirer.acquire();

        ResourceNode startingNode = getNode(startingRid);
        if (startingNode.aborted || edges[startingNode.index] == null) {
            mutex.release();
            return;
        }

        ArrayList<ResourceNode> cycle = new ArrayList<>();
        cycle.add(startingNode);
        ResourceNode currentNode = nodes[edges[startingNode.index]];

        while (currentNode.rid != startingRid && !currentNode.aborted && edges[currentNode.index] != null) {
            cycle.add(currentNode);
            currentNode = nodes[edges[currentNode.index]];
        }

        if (currentNode.rid == startingRid) {
            abortTransaction(cycle, transactionsInfo);
        }

        mutex.release();
    }
}
