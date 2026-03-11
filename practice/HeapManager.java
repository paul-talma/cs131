public class HeapManager {
    private static final int NULL = -1;
    public int[] memory;
    private int freeStart;

    public HeapManager(int[] initialMemory) {
        memory = initialMemory;
        memory[0] = memory.length;
        memory[1] = NULL;
        freeStart = 0;
    }

    public int allocate(int requestSize) {
        int allocatedSize = requestSize + 1;
        int lag = NULL;
        int p = freeStart;

        while (p != NULL && memory[p] < allocatedSize) { // search for block
            lag = p;
            p = memory[p + 1];
        }

        if (p == NULL) throw new OutOfMemoryError(); // no block large enough

        int nextFree = memory[p + 1];
        int remainingSize = memory[p] - allocatedSize;
        if (remainingSize > 1) {
            nextFree = p + allocatedSize;
            memory[nextFree] = remainingSize;
            memory[nextFree + 1] = memory[p + 1];
            memory[p] = allocatedSize;
        }

        if (lag == NULL) freeStart = nextFree;
        else memory[lag + 1] = nextFree;

        return p + 1;
    }

    public void deallocate(int address) {
        int addr = address - 1;
        int size = memory[addr];
        int p = freeStart;
        int lag = NULL;
        while (p != NULL && p < addr) {
            lag = p;
            p = memory[p + 1];
        }
        // TODO: check for out of bounds?
        if (addr + size == p) { // coalesce with next
            memory[addr] += memory[p];
            p = memory[p + 1]; // p points to next block
        }
        if (lag == NULL) {
            freeStart = addr;
            memory[addr + 1] = p;
        } else if (lag + memory[lag] == addr) {
            memory[lag] += memory[addr];
            memory[lag + 1] = p;
        } else {
            memory[lag + 1] = addr;
            memory[addr + 1] = p;
        }
    }
}
