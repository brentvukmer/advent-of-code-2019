package aoc.intcode.day7;

import java.util.List;
import java.util.ArrayList;

public class AmplifierChain {

    private List<IntCodeComputer> programChain;

    private AmplifierChain(int[] phaseSettings, int[] program) {
        programChain = new ArrayList<>();
        programChain.add( new IntCodeComputer(program, new int[] {phaseSettings[0]}));
        for (int i = 1; i < phaseSettings.length; i++) {
            programChain.add(new IntCodeComputer(program, new int[] {phaseSettings[i]}));
        }
    }

    private IntCodeComputer lastInChain() {
        return programChain.get(programChain.size() - 1);
    }

    public int run() {
        boolean halted =  false;
        int chainResult = -1;
        int input = 0;
        while (!halted) {
            for (IntCodeComputer comp : programChain) {
                halted = comp.run(input);
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
