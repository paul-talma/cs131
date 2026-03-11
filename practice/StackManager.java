public class StackManager {
  private int[] memory;
  private int top;

  public StackManager(int[] initialMemory) {
    memory = initialMemory;
    top = memory.length;
  }

  public int push(int requestSize) {
    int oldtop = top;
    top -= (requestSize + 1);
    if (top < 0) throw new StackOverflowError();
    memory[top] = oldtop;
    return top + 1;
  }

  public void pop() {
    top = memory[top];
  }
}
