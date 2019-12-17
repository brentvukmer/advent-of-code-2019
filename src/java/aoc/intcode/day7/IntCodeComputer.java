package aoc.intcode.day7;

import java.util.Arrays;

public class IntCodeComputer {

    private int[] program;
    private int instructionIndex;
    private int[] inputs;
    private int lastOutput;

    public IntCodeComputer(int[] program, int[] inputs) {
        this.program = Arrays.copyOf(program, program.length);
        this.instructionIndex = 0;
        this.inputs = inputs;
        lastOutput = -1;
    }

    public boolean run(int input) {
        inputs = new int[] {input, inputs[0]};
        return true;
    }

    public int getLastOutput() {
        return lastOutput;
    }

}

