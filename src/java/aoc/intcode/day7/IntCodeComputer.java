package aoc.intcode.day7;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class IntCodeComputer {

    private long[] program;
    private int programIndex;
    private long[] inputs;
    private int lastOutput;
    private boolean halted;
    private boolean outputWritten;

    public IntCodeComputer(long[] program, long[] inputs) {
        this.program = Arrays.copyOf(program, program.length);
        this.programIndex = 0;
        this.inputs = Arrays.copyOf(inputs, inputs.length);
        lastOutput = -1;
        halted = false;
        outputWritten = false;
    }

    private void add() {

    }

    private void multiply() {

    }

    private void readAndSave() {

    }

    private void output() {

    }

    private void jumpIfTrue() {

    }

    private void jumpIfFalse() {

    }

    private void lessThan() {

    }

    private void equals() {

    }

    private void runOp(int opcode, int numParams, List<Integer> paramModes) {
        System.out.println("IntCodeComputer#runOp: opCode=" + opcode);
        switch(opcode) {
            case 1:
                add();
                break;
            case 2:
                multiply();
                break;
            case 3:
                readAndSave();
                break;
            case 4:
                output();
                break;
            case 5:
                jumpIfTrue();
                break;
            case 6:
                jumpIfFalse();
                break;
            case 7:
                lessThan();
                break;
            case 8:
                equals();
                break;
            case 99:
                halted = true;
                break;
            default:
                throw new IllegalArgumentException("Invalid opcode: " + opcode);
        }
    }

    private int getNumParams(int opcode) {
        int result = -1;
        switch((int) opcode) {
            case 3:
                result = 1;
                break;
            case 4:
                result = 1;
                break;
            case 5:
                result = 2;
                break;
            case 6:
                result = 2;
                break;
            default:
                result = 3;
        }
        return result;
    }

    public boolean runProgram(long input) {
        inputs = new long[] {input, inputs[0]};
        int opCount = 0;
        System.out.println("IntCodeComputer#runProgram: START");
        while (!halted && !outputWritten) {
            System.out.println("IntCodeComputer#runProgram: programIndex=" + programIndex);
            long opCodeInput = program[programIndex];
            List<Integer> opCodeDigits = String
                    .valueOf(opCodeInput)
                    .chars()
                    .mapToObj(i -> (char) i)
                    .map(c -> Integer.parseInt(String.valueOf(c)))
                    .collect(Collectors.toList());
            System.out.println("IntCodeComputer#runProgram: opCodeDigits=" + opCodeDigits);
            int opCode = (opCodeDigits.size() >= 2) ?
                    Integer.parseInt(String.format("%d%d", opCodeDigits.get(opCodeDigits.size() - 2), opCodeDigits.get(opCodeDigits.size() - 1)))
                    :
                    opCodeDigits.get(0);
            System.out.println("IntCodeComputer#runProgram: opCode=" + opCode);
            int numParams = getNumParams(opCode);
            System.out.println("IntCodeComputer#runProgram: numParams=" + numParams);
            List<Integer> paramModeInputs = (opCodeDigits.size() > 2) ?
                opCodeDigits.subList(0, opCodeDigits.size() - 2)
                    :
                 new ArrayList<>();
            System.out.println("IntCodeComputer#runProgram: paramModeInputs=" + paramModeInputs);
            programIndex += 1 + numParams;
            opCount++;
            System.out.println("IntCodeComputer#runProgram: updated opCount=" + opCount);
        }
        System.out.println("IntCodeComputer#runProgram: END");
        return halted;
    }

    public int getLastOutput() {
        return lastOutput;
    }

    public boolean isHalted() {
        return halted;
    }

    public boolean outputWritten() { return outputWritten; }

}

