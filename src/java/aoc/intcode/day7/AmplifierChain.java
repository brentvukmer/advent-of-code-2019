package aoc.intcode.day7;

import java.util.List;
import java.util.ArrayList;

public class AmplifierChain {

    private List<IntCodeComputer> programChain;

    private AmplifierChain(Long[] phaseSettings, long[] program) {
        programChain = new ArrayList<>();
        programChain.add( new IntCodeComputer(program, new long[] {phaseSettings[0]}));
        for (int i = 1; i < phaseSettings.length; i++) {
            programChain.add(new IntCodeComputer(program, new long[] {phaseSettings[i]}));
        }
    }

    private IntCodeComputer lastInChain() {
        return programChain.get(programChain.size() - 1);
    }

    public int run() {
        boolean halted =  false;
        int chainResult = -1;
        int input = 0;
        int count = 0;
        while (!halted) {
            for (IntCodeComputer comp : programChain) {
                halted = comp.runProgram(input);
                count++;
                if (!halted) {
                    input = comp.getLastOutput();
                } else {
                    assert (comp.equals(lastInChain()));
                }
            }
        }
        return lastInChain().getLastOutput();
    }

}
