module day21;

import std.algorithm.iteration,
  std.algorithm.mutation,
  std.conv,
  std.file,
  std.range,
  std.regex,
  std.stdio,
  std.string,
  std.sumtype,
  std.typecons : tuple, Tuple;

immutable string sample = "
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
";

struct ConstantMonkey {
  string id;
  double value;
}

enum Operator { Plus, Minus, Multiplication, Division }

struct ArithmeticMonkey {
  string id;
  Operator operator;
  string leftId;
  string rightId;
}

alias Monkey = SumType!(ConstantMonkey, ArithmeticMonkey);

void main() {
  immutable string raw = readInput(true);
  Monkey[string] monkeys = parseInput(raw);
  solvePartOne(monkeys);
  solvePartTwo(monkeys);
}

string readInput(bool useFile) {
  if (useFile == true) {
    return readText("day21-input.txt");
  } else {
    return sample;
  }
}

Monkey[string] parseInput(string rawInput) {
  auto id = (string s) => s;

  string[] lines = rawInput.strip().splitLines();

  Monkey[string] monkeyMap;

  lines.each!((string s) {
      string[] parts = s.split(": ");
      string name = parts[0];
      string value = parts[1];
      auto m = matchFirst(value, regex(`\d+`));

      if (m.length > 0) {
        monkeyMap[name] = Monkey(ConstantMonkey(name, to!double(m[0])));
      } else {
        string[] expParts = value.strip().split(" ");
        string leftId = expParts[0];
        string rightId = expParts[2];
        Operator operator = parseOperator(expParts[1]);

        monkeyMap[name] = Monkey(ArithmeticMonkey(name, operator, leftId, rightId));
      }
    });

  return monkeyMap;
}

Operator parseOperator(string rawOperator) {
  switch (rawOperator) {
  case "/":
    return Operator.Division;
  case "*":
    return Operator.Multiplication;
  case "-":
    return Operator.Minus;
  case "+":
    return Operator.Plus;
  default:
    return Operator.Plus;
  }
}

// Part 1

void solvePartOne(Monkey[string] monkeys) {
  double partOne = calculateMonkeyValue(monkeys, "root");
  writeln("Part 1: ", format("%f", partOne));
}

double calculateMonkeyValue(Monkey[string] monkeys, string monkeyId) {
  Monkey currentMonkey = monkeys[monkeyId];
  return currentMonkey.match!(
                              (ConstantMonkey m) => m.value,
                              (ArithmeticMonkey m) => applyOperator(m.operator,
                                                                   calculateMonkeyValue(monkeys, m.leftId),
                                                                   calculateMonkeyValue(monkeys, m.rightId))
                              );
}

double applyOperator(Operator operator, double left, double right) {
  switch (operator) {
  case Operator.Division:
    return left / right;
  case Operator.Multiplication:
    return left * right;
  case Operator.Minus:
    return left - right;
  case Operator.Plus:
    return left + right;
  default:
    return left;
  }
}

// Part 2

enum Direction { Left, Right }

alias Operation = Tuple!(Operator, Direction, double);

alias Result = SumType!(double, Operation[]);

void solvePartTwo(Monkey[string] monkeys) {
  Monkey rootMonkey = monkeys["root"];

  auto resultPair = rootMonkey.match!(
                                      (ConstantMonkey m) => tuple(Result(to!double(0)), Result(to!double(0))),
                                      (ArithmeticMonkey m) => tuple(simplifyMonkey(monkeys, m.leftId),
                                                                   simplifyMonkey(monkeys, m.rightId))
                                      );

  Result leftResult = resultPair[0];
  Result rightResult = resultPair[1];

  double partTwo = leftResult.match!(
                                     (double value) => rightResult.match!(
                                                                         (double _r) => to!double(0),
                                                                         (Operation[] operations) => inverseOperations(operations.reverse, value)
                                                                         ),
                                     (Operation[] operations) => rightResult.match!(
                                                                                   (double value) => inverseOperations(operations.reverse, value),
                                                                                   (Operation[] _) => to!double(0)
                                                                                   )
                                     );

  writeln("Part 2: ", format("%f", partTwo)); // 3099532691300
}

immutable string humanId = "humn";

Result simplifyMonkey(Monkey[string] monkeys, string monkeyId) {
  Monkey currentMonkey = monkeys[monkeyId];

  auto doConstant = (ConstantMonkey m) {
    if (m.id == humanId) {
      return Result([]);
    } else {
      return Result(m.value);
    }
  };

  auto doArithmetic = (ArithmeticMonkey m) {
    Result leftResult = simplifyMonkey(monkeys, m.leftId);
    Result rightResult = simplifyMonkey(monkeys, m.rightId);

    return leftResult.match!(
                             (double leftVal) => rightResult.match!(
                                                                   (double rightVal) => Result(applyOperator(m.operator, leftVal, rightVal)),
                                                                   (Operation[] rightOps) => Result(rightOps ~ Operation(m.operator, Direction.Left, leftVal))
                                                                   ),
                             (Operation[] leftOps) => rightResult.match!(
                                                                        (double rightVal) => Result(leftOps ~ Operation(m.operator, Direction.Right, rightVal)),
                                                                        (Operation[] rightOps) => Result(leftOps ~ rightOps)
                                                                        )
                             );
  };

  Result result = currentMonkey.match!(
                                       (ConstantMonkey m) => doConstant(m),
                                       (ArithmeticMonkey m) => doArithmetic(m)
                                       );

  return result;
}

double inverseOperations(Operation[] operations, double value) {
  double resultValue = value;

  foreach (Operation operation; operations) {
    Operator operator = operation[0];
    Direction direction = operation[1];
    double opVal = operation[2];

    switch (operator) {
    case Operator.Division:
      if (direction == Direction.Left) {
        resultValue = opVal / resultValue;
      } else {
        resultValue = resultValue * opVal;
      }
      break;

    case Operator.Multiplication:
      resultValue = resultValue / opVal;
      break;

    case Operator.Minus:
      if (direction == Direction.Left) {
        resultValue = opVal - resultValue;
      } else {
        resultValue = resultValue + opVal;
      }
      break;

    case Operator.Plus:
      resultValue = resultValue - opVal;
      break;

    default:
      break;
    }
  }

  return resultValue;
}
