package cp1.solution;

import cp1.base.ActiveTransactionAborted;
import cp1.base.Resource;
import cp1.base.ResourceId;

import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Class containing most needed information about the resource possessed by
 * Transaction Manager.
 */
public class ResourceInfo {
    // ThreadId of thread currently owning the resource, set to 0 if resource
    // has no owner.
    public volatile long owner;
    public ResourceId rid;
    public Resource resource;
    private final AtomicInteger awaitingCount;
    // Semaphore synchronizing thread owning resource and the one to which
    // resource is passed after commit or rollback.
    public final Semaphore awakeningMutex;
    // Semaphore synchronizing threads awaiting for this resource.
    private final Semaphore awaitingForResource;
    private final Semaphore mutex;
    private final ForceMutexAcquire forceAcquirer;

    ResourceInfo(Resource resource) {
        owner = 0L;
        rid = resource.getId();
        this.resource = resource;
        awaitingCount = new AtomicInteger(0);
        awakeningMutex = new Semaphore(0, true);
        awaitingForResource = new Semaphore(0, true);
        mutex = new Semaphore(1, true);
        forceAcquirer = new ForceMutexAcquire(mutex);
    }

    /**
     * Increments or decrements 'awaitingCount' based on 'increment' value.
     * @param increment increment if equals 'true', otherwise decrement.
     */
    public void changeAwaitingCount(boolean increment) {
        forceAcquirer.acquire();

        if (increment) {
            awaitingCount.getAndIncrement();
        }
        else {
            awaitingCount.getAndDecrement();
        }

        mutex.release();
    }

    public void awaitForResource(Semaphore mutex, TransactionInfo transaction, ResourcesDependencyGraph rdGraph)
            throws ActiveTransactionAborted, InterruptedException {
        changeAwaitingCount(true);
        mutex.release();

        try {
            awaitingForResource.acquire();
        }
        catch (InterruptedException e) {
            if (transaction.getAbortedValue()) {
                changeAwaitingCount(false);
                Thread.currentThread().interrupt();
                throw new ActiveTransactionAborted();
            }
            else {
                changeAwaitingCount(false);
                rdGraph.updateEdges(transaction.ownedResources);
                throw e;
            }
        }
    }

    /**
     * If some thread is awaiting for this resource function releases it and
     * waits until released thread obtains this resource.
     */
    void releaseAwaitingForResource() {
        if (getAwaitingCount() > 0) {
            awaitingForResource.release();

            boolean mutexAcquired = false;
            boolean threadInterrupted = false;

            while (!mutexAcquired) {
                try {
                    awakeningMutex.acquire();
                    mutexAcquired = true;
                }
                catch (InterruptedException e) {
                    threadInterrupted = true;
                }
            }

            if (threadInterrupted) {
                Thread.currentThread().interrupt();
            }
        }
    }

    public long getAwaitingCount() {
        forceAcquirer.acquire();

        long count = awaitingCount.get();

        mutex.release();

        return count;
    }
}
