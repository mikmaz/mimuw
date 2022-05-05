package cp1.solution;

import cp1.base.Resource;
import cp1.base.ResourceId;

import java.util.Collection;
import java.util.concurrent.Semaphore;

/**
 * Class for managing resources possessed by Transaction Manager.
 */
public class ResourcesManager {
    private final ResourceInfo[] resourcesInfo;
    private final Semaphore mutex;
    private final ForceMutexAcquire forceAcquirer;

    public ResourcesManager(Collection<Resource> resources) {
        resourcesInfo = new ResourceInfo[resources.size()];
        int i = 0;
        for (Resource resource : resources) {
            resourcesInfo[i] = new ResourceInfo(resource);
            i++;
        }

        mutex = new Semaphore(1, true);
        forceAcquirer = new ForceMutexAcquire(mutex);
    }

    public int getResourcesInfoLen() {
        return resourcesInfo.length;
    }

    public void setResourceDependencyGraph(ResourcesDependencyGraph.ResourceNode[] nodes, Integer[] edges) {
        int resourcesLen = resourcesInfo.length;
        for (int i = 0; i < resourcesLen; i++) {
            nodes[i] = new ResourcesDependencyGraph.ResourceNode(resourcesInfo[i].rid, i);
            edges[i] = null;
        }
    }

    public void updateResourcesAfterTransaction(TransactionInfo transaction, ResourcesDependencyGraph rdGraph) {
        ResourceInfo rInfo;
        for (ResourceId rid : transaction.ownedResources) {
            rInfo = getResource(rid);
            rInfo.owner = 0;
            rdGraph.updateNodeInfo(rInfo.rid, false, 0, 0);
            rInfo.releaseAwaitingForResource();
        }
    }

    public ResourceInfo getResource(ResourceId rid) {
        forceAcquirer.acquire();

        for (ResourceInfo rInfo : resourcesInfo) {
            if (rInfo.resource.getId() == rid) {
                mutex.release();
                return rInfo;
            }
        }

        mutex.release();
        return null;
    }
}
