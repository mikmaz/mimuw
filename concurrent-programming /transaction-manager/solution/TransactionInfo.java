package cp1.solution;

import cp1.base.LocalTimeProvider;
import cp1.base.Resource;
import cp1.base.ResourceId;
import cp1.base.ResourceOperation;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.Semaphore;

/**
 * Class for storing information about currently active transactions in
 * Transaction Manager.
 */
public class TransactionInfo {
    private static class OperationPair {
        public Resource resource;
        public ResourceOperation operation;

        OperationPair(Resource resource, ResourceOperation operation) {
            this.resource = resource;
            this.operation = operation;
        }
    }

    public volatile boolean aborted_exception_thrown;
    public ConcurrentLinkedDeque<OperationPair> operations;
    public Thread thread;
    public ArrayList<ResourceId> ownedResources;
    public long startingTime;
    private volatile boolean aborted;
    private final Semaphore mutex;
    private final ForceMutexAcquire forceAcquirer;

    TransactionInfo(LocalTimeProvider timeProvider) {
        aborted = false;
        operations = new ConcurrentLinkedDeque<>();
        thread = Thread.currentThread();
        ownedResources = new ArrayList<>();
        startingTime = timeProvider.getTime();
        aborted_exception_thrown = false;
        mutex = new Semaphore(1, true);
        forceAcquirer = new ForceMutexAcquire(mutex);
    }

    public void addNewOperationPair(Resource resource, ResourceOperation operation) {
        operations.add(new OperationPair(resource, operation));
    }

    public void undoOperations() {
        OperationPair operationOnResource;
        while (!operations.isEmpty()) {
            operationOnResource = operations.removeLast();
            operationOnResource.operation.undo(operationOnResource.resource);
        }
    }

    public void abortTransaction() {
        forceAcquirer.acquire();

        aborted = true;
        thread.interrupt();

        mutex.release();
    }

    public boolean getAbortedValue() {
        forceAcquirer.acquire();

        boolean ret = aborted;
        mutex.release();
        return ret;
    }
}
