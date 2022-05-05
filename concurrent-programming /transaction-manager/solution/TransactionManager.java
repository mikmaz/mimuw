package cp1.solution;

import cp1.base.*;

import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;

public class TransactionManager implements cp1.base.TransactionManager {
    private final ConcurrentHashMap<Long, TransactionInfo> transactionsInfo;
    private final ResourcesManager resources;
    private final ResourcesDependencyGraph rdGraph;
    private final Semaphore mutex;
    private final LocalTimeProvider timeProvider;
    private final ForceMutexAcquire forceAcquirer;

    public TransactionManager(Collection<Resource> resources, LocalTimeProvider timeProvider) {
        transactionsInfo = new ConcurrentHashMap<>();
        this.resources = new ResourcesManager(resources);
        mutex = new Semaphore(1, true);
        rdGraph = new ResourcesDependencyGraph(this.resources);
        this.timeProvider = timeProvider;
        forceAcquirer = new ForceMutexAcquire(mutex);
    }

    public void startTransaction() throws AnotherTransactionActiveException {
        long threadId = Thread.currentThread().getId();
        if (isTransactionActive()) {
            throw new AnotherTransactionActiveException();
        }
        else {
            transactionsInfo.put(threadId, new TransactionInfo(timeProvider));
        }
    }

    public void operateOnResourceInCurrentTransaction(ResourceId rid, ResourceOperation operation)
            throws NoActiveTransactionException, UnknownResourceIdException, ActiveTransactionAborted,
            ResourceOperationException, InterruptedException {
        long threadId = Thread.currentThread().getId();
        TransactionInfo transaction = transactionsInfo.get(threadId);

        if (transaction == null) {
            throw new NoActiveTransactionException();
        }

        ResourceInfo rInfo = resources.getResource(rid);

        if (rInfo == null) {
            throw new UnknownResourceIdException(rid);
        }

        if (transaction.getAbortedValue()) {
            throw new ActiveTransactionAborted();
        }

        mutex.acquire();

        if (rInfo.owner == 0) {
            rInfo.owner = threadId;
            transaction.ownedResources.add(rid);
            rdGraph.updateNodeInfo(rid, false, threadId, transaction.startingTime);
            mutex.release();
        }
        else if (rInfo.owner != threadId) {
            rdGraph.addTransaction(transaction, rid);
            rdGraph.checkDeadlock(rid, transactionsInfo);
            rInfo.awaitForResource(mutex, transaction, rdGraph);

            rInfo.changeAwaitingCount(false);
            rInfo.owner = threadId;
            transaction.ownedResources.add(rid);
            rdGraph.updateNodeInfo(rid, false, threadId, transaction.startingTime);
            rdGraph.updateEdges(transaction.ownedResources);
            rInfo.awakeningMutex.release();
        }
        else {
            mutex.release();
        }

        if (Thread.interrupted()) {
            throw new InterruptedException();
        }

        operation.execute(rInfo.resource);

        if (Thread.interrupted()) {
            operation.undo(rInfo.resource);
            throw new InterruptedException();
        }

        transaction.addNewOperationPair(rInfo.resource, operation);
    }

    public void commitCurrentTransaction() throws NoActiveTransactionException, ActiveTransactionAborted {
        forceAcquirer.acquire();

        long threadId = Thread.currentThread().getId();
        TransactionInfo transaction = transactionsInfo.get(threadId);

        if (transaction == null) {
            mutex.release();
            throw new NoActiveTransactionException();
        }
        else if (transaction.getAbortedValue()) {
            mutex.release();
            throw new ActiveTransactionAborted();
        }

        resources.updateResourcesAfterTransaction(transaction, rdGraph);
        transactionsInfo.remove(threadId);

        mutex.release();
    }

    public void rollbackCurrentTransaction() {
        forceAcquirer.acquire();

        long threadId = Thread.currentThread().getId();
        TransactionInfo transaction = transactionsInfo.get(threadId);

        if (transaction == null) {
            mutex.release();
            return;
        }

        transaction.undoOperations();

        rdGraph.updateEdges(transaction.ownedResources);
        resources.updateResourcesAfterTransaction(transaction, rdGraph);
        transactionsInfo.remove(threadId);

        mutex.release();
    }

    public boolean isTransactionActive() {
        return transactionsInfo.containsKey(Thread.currentThread().getId());
    }

    public boolean isTransactionAborted() {
        return transactionsInfo.get(Thread.currentThread().getId()).getAbortedValue();
    }
}
