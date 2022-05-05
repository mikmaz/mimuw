package cp1.solution;

import java.util.concurrent.Semaphore;

/**
 * Class for performing acquire operation on Semaphore even if thread gets
 * interrupted while waiting.
 */
public class ForceMutexAcquire {
    private final Semaphore mutex;

    public ForceMutexAcquire(Semaphore mutex) {
        this.mutex = mutex;
    }

    public void acquire() {
        boolean mutexAcquired = false;
        boolean threadInterrupted = false;

        while (!mutexAcquired) {
            try {
                mutex.acquire();
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
