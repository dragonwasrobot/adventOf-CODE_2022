import "dart:io";

const sampleInput = """
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
""";


List<String> parseInput(bool useFile) {
  String rawInput;

  if (useFile) {
    File inputFile = File("day16-input.txt");
    rawInput = inputFile.readAsStringSync();
  } else {
    rawInput = sampleInput;
  }

  return rawInput.trim().split('\n');
}

class Valve {
  final String id;
  final int index;
  final int flowRate;
  final List<String> tunnels;

  Valve(this.id, this.index, this.flowRate, this.tunnels);

  @override
  String toString() {
    return '{id: ${id}, index: ${index}, flowRate: ${flowRate}, tunnels: ${tunnels}}';
  }
}

Map<String, Valve> parseValves(List<String> valveStrs) {
  RegExp configRegex = RegExp(r'Valve (?<id>\w+) has flow rate=(?<flowRate>\d+); (tunnel|tunnels) (lead|leads) to (valve|valves) (?<tunnels>.*)');

  Map<String, Valve> valves = Map();
  valveStrs.sort();

  for (int i = 0; i < valveStrs.length; i++) {
    String valveStr = valveStrs[i];
    RegExpMatch? match = configRegex.firstMatch(valveStr);

    if (match != null) {
      String id = match.namedGroup('id') ?? "";
      int flowRate = int.parse(match.namedGroup('flowRate') ?? "0");

      List<String> tunnels = (match.namedGroup('tunnels') ?? "").split(", ");

      Valve valve = Valve(id, i, flowRate, tunnels);
      valves[valve.id] = valve;
    }
  }

  return valves;
}

const maxDist = 1000000;

List<List<int>> computeFloydWarshall(Map<String, Valve> valves) {

  List<List<int>> distances = List.generate(valves.length, (i) =>
    new List<int>.from(List<int>.filled(valves.length, maxDist))
  );

  for (var valve in valves.values) {
    distances[valve.index][valve.index] = 0;

    for (var tunnelId in valve.tunnels) {
      var tunnelIndex = valves[tunnelId]!.index;
      distances[valve.index][tunnelIndex] = 1;
    }
  }

  for (int k = 0; k < valves.length; k++) {
    for (int i = 0; i < valves.length; i++) {
      for (int j = 0; j < valves.length; j++) {
        if (distances[i][j] > distances[i][k] + distances[k][j]) {
          distances[i][j] = distances[i][k] + distances[k][j];
        }
      }
    }
  }

  return distances;
}

class Configuration {
  final List<Valve> valves;
  final List<List<int>> distances;

  Configuration(this.valves, this.distances);

  @override
  String toString() {
    return '{valves: ${valves}, distances: ${distances}}';
  }
}

Configuration initializeConfiguration(Map<String, Valve> valvesMap, List<List<int>> distances) {

  List<Valve> valves = valvesMap.values.toList();
  List<int> ignoredValves = valves.where((valve) =>
    valve.flowRate == 0 && valve.id != "AA").map((v)
    => v.index
  ).toList();

  ignoredValves.sort();
  for (var ignoredValveIdx in ignoredValves.reversed) {
    for (int i = 0; i < distances.length; i++) {
      distances[i].removeAt(ignoredValveIdx);
    }
    distances.removeAt(ignoredValveIdx);
    valves.removeAt(ignoredValveIdx);
  }

  return Configuration(valves, distances);
}

int computeMaxPressurePartOne(Configuration configuration) {
  List<List<int>> distances = configuration.distances;
  List<Valve> valves = configuration.valves;

  const totalTime = 30;
  int max = 0;

  void doComputePath(List<int> currentValves,
                   List<int> candidateValves,
                   int timeLeft) {

    int currentIdx = currentValves.last;
    List<int> currentRow = distances[currentIdx];

    if (candidateValves.length == 0) {
      int result = computeReleasedPressure(currentValves, totalTime, distances, valves);
      if (result > max) {
        max = result;
      }
      return;
    }

    for (var candidateIdx in candidateValves) {
      int timeDist = currentRow[candidateIdx];

      if (timeLeft - timeDist > 0) {
        int newTimeLeft = timeLeft - timeDist;

        List<int> newCandidateValves = new List.from(candidateValves);
        newCandidateValves.remove(candidateIdx);

        List<int> newCurrentValves = new List.from(currentValves);
        newCurrentValves.add(candidateIdx);

        doComputePath(newCurrentValves, newCandidateValves, newTimeLeft);
      } else {
        int result = computeReleasedPressure(currentValves, totalTime, distances, valves);
        if (result > max) {
          max = result;
        }
      }
    }
  }

  List<int> seedPath = List.generate(distances.length - 1, (i) => i + 1);
  doComputePath([0], seedPath, 30);

  return max;
}

int computeMaxPressurePartTwo(Configuration configuration) {
  List<List<int>> distances = configuration.distances;
  List<Valve> valves = configuration.valves;

  const totalTime = 26;
  int max = 0;

  void doComputePath(List<int> ownValves,
                     int ownTimeLeft,
                     List<int> elephantValves,
                     int elephantTimeLeft,
                     List<int> candidateValves) {

    if (candidateValves.length == 0) {
      int ownResult = computeReleasedPressure(ownValves, totalTime, distances, valves);
      int elephantResult = computeReleasedPressure(elephantValves, totalTime, distances, valves);
      int result = ownResult + elephantResult;

      if (result > max) {
        max = result;
      }

      return;
    }

    // Pick own valve
    int currentOwnIdx = ownValves.last;
    List<int> currentOwnRow = distances[currentOwnIdx];

    for (var ownCandidateIdx in candidateValves) {
      int timeDist = currentOwnRow[ownCandidateIdx];

      List<int> newCandidateValves = new List.from(candidateValves);
      List<int> newOwnValves = new List.from(ownValves);
      int newOwnTimeLeft = ownTimeLeft;

      if (ownTimeLeft - timeDist > 0) {
        newOwnTimeLeft = ownTimeLeft - timeDist;

        newCandidateValves.remove(ownCandidateIdx);
        newOwnValves.add(ownCandidateIdx);
      }

      // Pick elephant valve
      int currentElephantIdx = elephantValves.last;
      List<int> currentElephantRow = distances[currentElephantIdx];

      for (var elephantCandidateIdx in newCandidateValves) {
        int timeDist = currentElephantRow[elephantCandidateIdx];

        if (elephantTimeLeft - timeDist > 0) {
          int newElephantTimeLeft = elephantTimeLeft - timeDist;

          List<int> newerCandidateValves = new List.from(newCandidateValves);
          newerCandidateValves.remove(elephantCandidateIdx);

          List<int> newElephantValves = new List.from(elephantValves);
          newElephantValves.add(elephantCandidateIdx);

          doComputePath(newOwnValves, newOwnTimeLeft, newElephantValves, newElephantTimeLeft, newerCandidateValves);
        } else {
          int ownResult = computeReleasedPressure(newOwnValves, totalTime, distances, valves);
          int elephantResult = computeReleasedPressure(elephantValves, totalTime, distances, valves);
          int result = ownResult + elephantResult;
          if (result > max) {
            max = result;
          }
        }
      }
    }
  }

  List<int> seedPath = List.generate(distances.length - 1, (i) => i + 1);
  doComputePath([0], totalTime, [0], totalTime, seedPath);

  return max;
}

int computeReleasedPressure(List<int> path,
                            int timeLeft,
                            List<List<int>> distances,
                            List<Valve> valves) {

 int releasedPressure = 0;
 int valveIdx = 0;

 for (int i = 1; i < path.length; i++) {
   List<int> distList = distances[valveIdx];

   int nextValveIdx = path[i];
   Valve nextValve = valves[nextValveIdx];

   int distToValve = distList[nextValveIdx];

   timeLeft = timeLeft - (distToValve + 1);
   if (timeLeft < 0) {
     return releasedPressure;
   } else {
     releasedPressure = releasedPressure + (timeLeft * nextValve.flowRate);
      valveIdx = nextValveIdx;
   }
 }

 return releasedPressure;
}

void main() {
  bool useFile = true;
  List<String> input = parseInput(useFile);
  Map<String, Valve> valvesMap = parseValves(input);
  List<List<int>> distances = computeFloydWarshall(valvesMap);
  Configuration configuration = initializeConfiguration(valvesMap, distances);

  int answer1 = computeMaxPressurePartOne(configuration);
  print("Answer 1");
  print(answer1); // 2080

  int answer2 = computeMaxPressurePartTwo(configuration);
  print("Answer 2");
  print(answer2); // 2752
}
