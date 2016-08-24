
import streams.Bloxorz.Level1

Level1.pathsFromStart.toList
//Level1.from(Level1.pathsFromStart, Set()).take(6).toList
Level1.pathsToGoal.take(6).toList
//Level1.pathsToGoal.filter(p => Level1.done(p._1)).toList