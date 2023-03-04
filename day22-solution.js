#!/usr/bin/env node

const fs = require("fs");

const sampleInput = `
        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
`;

const readInput = (useFile) => {
  if (useFile) {
    return fs.readFileSync(__dirname + "/day22-input.txt", "utf8");
  } else {
    return sampleInput;
  }
};

const padArray = (arr, len, fill) =>
      arr.concat(Array(len).fill(fill)).slice(0, len);

const parseCaveBoard = (stringCaveBoard) => {
  let caveBoard = stringCaveBoard
      .split("\n")
      .filter((line) => line.length > 0)
      .map((line) => line.split(""));

  const rows = caveBoard.length;
  const cols = Math.max(...caveBoard.map((row) => row.length));
  const dimensions = { rows: rows, cols: cols };

  caveBoard = caveBoard.map((row) => {
    if (row.length < cols) {
      return padArray(row, cols, " ");
    } else {
      return row;
    }
  });

  // A square in a cave can either be:
  // Nothing: ' '
  // Tile: '.'
  // Wall: '#'
  return [caveBoard, dimensions];
};

const parseInstructions = (stringInstructions) => {
  // An instruction is either an integer, an 'L' or a 'R'.
  return stringInstructions
    .replaceAll("R", " R ")
    .replaceAll("L", " L ")
    .split(" ")
    .map((s) => (["R", "L"].indexOf(s) >= 0 ? s : parseInt(s)));
};

const parseInput = (rawInput) => {
  const parts = rawInput.split("\n\n");
  const [caveBoard, dimensions] = parseCaveBoard(parts[0]);
  const instructions = parseInstructions(parts[1]);
  return [caveBoard, dimensions, instructions];
};

const startingPosition = (caveBoard) => {
  let col = caveBoard[0].findIndex((col) => col == ".");
  return { row: 0, col: col };
};

const turnDirection = (state, direction) => {
  switch (direction) {
  case "L":
    state.direction = (state.direction + 90) % 360;
    break;

  case "R":
    state.direction =
      state.direction - 90 < 0
      ? state.direction - 90 + 360
      : state.direction - 90;
    break;

  default:
    console.log(`Unexpected direction instruction: ${direction}`);
    break;
  }
};

const moveEast = (caveBoard, dimensions, position, steps, isCube) => {
  let stepsLeft = steps;
  let currentPosition = position;
  let newDirection = 0;

  while (true) {
    if (stepsLeft == 0) {
      break;
    }
    stepsLeft--;

    let candidateTile;
    let candidateDirection = 0;

    if (isCube && 0 <= currentPosition.row && currentPosition.row < 50 && currentPosition.col == 149) {
      candidateTile = {
        row: 149 - currentPosition.row,
        col: 99,
        tile: caveBoard[149 - currentPosition.row][99]
      };
      candidateDirection = 180;

    } else if (isCube && 50 <= currentPosition.row && currentPosition.row < 100 && currentPosition.col == 99) {
      candidateTile = {
        row: 49,
        col: 50 + currentPosition.row,
        tile: caveBoard[49][50 + currentPosition.row]
      };
      candidateDirection = 90;

    } else if (isCube && 100 <= currentPosition.row && currentPosition.row < 150 && currentPosition.col == 99) {
      candidateTile = {
        row: 149 - currentPosition.row,
        col: 149,
        tile: caveBoard[149 - currentPosition.row][149]
      };
      candidateDirection = 180;

    } else if (isCube && 150 <= currentPosition.row && currentPosition.row < 200 && currentPosition.col == 49) {
      candidateTile = {
        row: 149,
        col: currentPosition.row - 100,
        tile: caveBoard[149][currentPosition.row - 100]
      };
      candidateDirection = 90;

    } else if (!isCube &&
      currentPosition.col + 1 >= dimensions.cols ||
        caveBoard[currentPosition.row][currentPosition.col + 1] == " "
    ) {
      let candidateCol = caveBoard[currentPosition.row].findIndex(
        (col) => col != " "
      );
      candidateTile = {
        row: currentPosition.row,
        col: candidateCol,
        tile: caveBoard[currentPosition.row][candidateCol],
      };

    } else {
      candidateTile = {
        row: currentPosition.row,
        col: currentPosition.col + 1,
        tile: caveBoard[currentPosition.row][currentPosition.col + 1],
      };
    }

    if (candidateTile.tile == ".") {
      // Move forward
      currentPosition.row = candidateTile.row;
      currentPosition.col = candidateTile.col;
      newDirection = candidateDirection;
    } else if (candidateTile.tile == "#") {
      // Hit a wall
      break;
    } else {
      console.log(`Unexpected tile: ${JSON.stringify(candidateTile)}`);
    }
  }

  return [currentPosition, newDirection];
};

const moveWest = (caveBoard, dimensions, position, steps, isCube) => {
  let stepsLeft = steps;
  let currentPosition = position;
  let newDirection = 180;

  const findLastNonEmptyColumn = (rowIdx) => {
    for (let i = 0; i < dimensions.cols; i++) {
      let colIdx = dimensions.cols - 1 - i;
      if (caveBoard[rowIdx][colIdx] != " ") {
        return colIdx;
      }
    }

    console.log("Couldn't find last nonempty column!");
    return -1;
  };

  while (true) {
    if (stepsLeft == 0) {
      break;
    }
    stepsLeft--;

    let candidateTile;
    let candidateDirection = 180;

    if (isCube && 0 <= currentPosition.row && currentPosition.row < 50 && currentPosition.col == 50) {
      candidateTile = {
        row: 149 - currentPosition.row,
        col: 0,
        tile: caveBoard[149 - currentPosition.row][0]
      };
      candidateDirection = 0;

    } else if (isCube && 50 <= currentPosition.row && currentPosition.row < 100 && currentPosition.col == 50) {
      candidateTile = {
        row: 100,
        col: currentPosition.row - 50,
        tile: caveBoard[100][currentPosition.row - 50]
      };
      candidateDirection = 270;

    } else if (isCube && 100 <= currentPosition.row && currentPosition.row < 150 && currentPosition.col == 0) {
      candidateTile = {
        row: 149 - currentPosition.row,
        col: 50,
        tile: caveBoard[149 - currentPosition.row][50]
      };
      candidateDirection = 0;

    } else if (isCube && 150 <= currentPosition.row && currentPosition.row < 200 && currentPosition.col == 0) {
      candidateTile = {
        row: 0,
        col: currentPosition.row - 100,
        tile: caveBoard[0][currentPosition.row - 100]
      };
      candidateDirection = 270;

    } else if (!isCube &&
      currentPosition.col - 1 < 0 ||
        caveBoard[currentPosition.row][currentPosition.col - 1] == " "
    ) {
      let candidateCol = findLastNonEmptyColumn(currentPosition.row);
      candidateTile = {
        row: currentPosition.row,
        col: candidateCol,
        tile: caveBoard[currentPosition.row][candidateCol],
      };

    } else {
      candidateTile = {
        row: currentPosition.row,
        col: currentPosition.col - 1,
        tile: caveBoard[currentPosition.row][currentPosition.col - 1],
      };
    }

    if (candidateTile.tile == ".") {
      // Move forward
      currentPosition.row = candidateTile.row;
      currentPosition.col = candidateTile.col;
      newDirection = candidateDirection;
    } else if (candidateTile.tile == "#") {
      // Hit a wall
      break;
    } else {
      console.log(`Unexpected tile: ${JSON.stringify(candidateTile)}`);
    }
  }

  return [currentPosition, newDirection];
};

const moveNorth = (caveBoard, dimensions, position, steps, isCube) => {
  let stepsLeft = steps;
  let currentPosition = position;
  let newDirection = 90;

  const findLastNonEmptyRow = (colIdx) => {
    for (let i = 0; i < dimensions.rows; i++) {
      let rowIdx = dimensions.rows - 1 - i;
      if (caveBoard[rowIdx][colIdx] != " ") {
        return rowIdx;
      }
    }

    console.log("Couldn't find last nonempty row!");
    return -1;
  };

  while (true) {
    if (stepsLeft == 0) {
      break;
    }
    stepsLeft--;

    let candidateTile;
    let candidateDirection = 90;

    if (isCube && 0 == currentPosition.row && 100 <= currentPosition.col && currentPosition.col < 150) {
      candidateTile = {
        row: 199,
        col: currentPosition.col - 100,
        tile: caveBoard[199][currentPosition.col - 100]
      };
      candidateDirection = 90;

    } else if (isCube && 0 == currentPosition.row && 50 <= currentPosition.col && currentPosition.col < 100) {
        candidateTile = {
          row: 100 + currentPosition.col,
          col: 0,
          tile: caveBoard[100 + currentPosition.col][0]
        };
        candidateDirection = 0;

    } else if (isCube && 100 == currentPosition.row && 0 <= currentPosition.col && currentPosition.col < 50) {
      candidateTile = {
        row: 50 + currentPosition.col,
        col: 50,
        tile: caveBoard[50 + currentPosition.col][50]
      };
      candidateDirection = 0;

    } else if (!isCube &&
      currentPosition.row - 1 < 0 ||
        caveBoard[currentPosition.row - 1][currentPosition.col] == " "
    ) {
      let candidateRowIdx = findLastNonEmptyRow(currentPosition.col);

      candidateTile = {
        row: candidateRowIdx,
        col: currentPosition.col,
        tile: caveBoard[candidateRowIdx][currentPosition.col],
      };
    } else {
      candidateTile = {
        row: currentPosition.row - 1,
        col: currentPosition.col,
        tile: caveBoard[currentPosition.row - 1][currentPosition.col],
      };
    }

    if (candidateTile.tile == ".") {
      // Move forward
      currentPosition.row = candidateTile.row;
      currentPosition.col = candidateTile.col;
      newDirection = candidateDirection;
    } else if (candidateTile.tile == "#") {
      // Hit a wall
      break;
    } else {
      console.log(`Unexpected tile: ${JSON.stringify(candidateTile)}`);
    }
  }

  return [currentPosition, newDirection];
};

const moveSouth = (caveBoard, dimensions, position, steps, isCube) => {
  let stepsLeft = steps;
  let currentPosition = position;
  let newDirection = 270;

  const findFirstNonEmptyRow = (colIdx) => {
    for (let i = 0; i < dimensions.rows; i++) {
      let rowIdx = i;
      if (caveBoard[rowIdx][colIdx] != " ") {
        return rowIdx;
      }
    }

    console.log("Couldn't find first nonempty row!");
    return -1;
  };

  while (true) {
    if (stepsLeft == 0) {
      break;
    }
    stepsLeft--;

    let candidateTile;
    let candidateDirection = 270;

    if (isCube && 49 == currentPosition.row && 100 <= currentPosition.col && currentPosition.col < 150) {
      candidateTile = {
        row: currentPosition.col - 50,
        col: 99,
        tile: caveBoard[currentPosition.col - 50][99]
      };
      candidateDirection = 0;

    } else if (isCube && 149 == currentPosition.row && 50 <= currentPosition.col && currentPosition.col < 100) {
      candidateTile = {
        row: 100 + currentPosition.col,
        col: 49,
        tile: caveBoard[100 + currentPosition.col][49]
      };
      candidateDirection = 180;

    } else if (isCube && 199 == currentPosition.row && 0 <= currentPosition.col && currentPosition.col < 50) {
      candidateTile = {
        row: 0,
        col: 100 + currentPosition.col,
        tile: caveBoard[0][100 + currentPosition.col]
      };
      candidateDirection = 270;

    } else if (!isCube &&
      currentPosition.row + 1 >= dimensions.rows ||
        caveBoard[currentPosition.row + 1][currentPosition.col] == " "
    ) {
      let candidateRowIdx = findFirstNonEmptyRow(currentPosition.col);

      candidateTile = {
        row: candidateRowIdx,
        col: currentPosition.col,
        tile: caveBoard[candidateRowIdx][currentPosition.col],
      };
    } else {
      candidateTile = {
        row: currentPosition.row + 1,
        col: currentPosition.col,
        tile: caveBoard[currentPosition.row + 1][currentPosition.col],
      };
    }

    if (candidateTile.tile == ".") {
      // Move forward
      currentPosition.row = candidateTile.row;
      currentPosition.col = candidateTile.col;
      newDirection = candidateDirection;
    } else if (candidateTile.tile == "#") {
      // Hit a wall
      break;
    } else {
      console.log(`Unexpected tile: ${JSON.stringify(candidateTile)}`);
    }
  }

  return [currentPosition, newDirection];
};

const moveSteps = (caveBoard, dimensions, state, steps, isCube) => {
  let newPosition;

  switch (state.direction) {
  case 0:
    [newPosition, newDirection] = moveEast(caveBoard, dimensions, state.position, steps, isCube);
    break;

  case 90:
    [newPosition, newDirection] = moveNorth(caveBoard, dimensions, state.position, steps, isCube);
    break;

  case 180:
    [newPosition, newDirection] = moveWest(caveBoard, dimensions, state.position, steps, isCube);
    break;

  case 270:
    [newPosition, newDirection] = moveSouth(caveBoard, dimensions, state.position, steps, isCube);
    break;

  default:
    console.log(`Unexpected direction state: ${direction}`);
    break;
  }

  state.position = newPosition;
  state.direction = newDirection;
};

const walkMap = (caveBoard, dimensions, instructions, isCube) => {

  let state = { position: startingPosition(caveBoard), direction: 0 };

  instructions.forEach((instruction) => {
    switch (typeof instruction) {
    case "string":
      turnDirection(state, instruction);
      break;

    case "number":
      moveSteps(caveBoard, dimensions, state, instruction, isCube);
      break;

    default:
      console.log(`Unexpected instruction: ${instruction}`);
      break;
    }
  });

  const calculateDirection = (direction) => {
    switch (direction) {
    case 0:
      return 0;
    case 90:
      return 3;
    case 180:
      return 2;
    case 270:
      return 1;
    }
  };

  const finalPassword =
        (state.position.row + 1) * 1000 +
        (state.position.col + 1) * 4 +
        calculateDirection(state.direction);

  return finalPassword;
};

const main = () => {
  const rawInput = readInput(true);
  const [caveBoard, dimensions, instructions] = parseInput(rawInput);

  const partOne = walkMap(caveBoard, dimensions, instructions, false);
  console.log(`Part one: ${partOne}`); // 56_372

  const partTwo = walkMap(caveBoard, dimensions, instructions, true);
  console.log(`Part two: ${partTwo}`); // 197_047
};

main();
