// Day18

class Cube {
    int x
    int y
    int z
    boolean isLava

    String toString() {
        "Lava: ${isLava}, Pos: {${x}, ${y}, ${z}}"
    }
}

class Day18 {

    static void main(String[] args) {
        println("Running...")
        boolean useFile = true
        List<Cube> lavaCubes = parseInput(useFile)
        Cube[][][] space = initializeSpace(lavaCubes)

        calculatePart1(space, lavaCubes)
        calculatePart2(space, lavaCubes)
    }

    static List<Cube> parseInput(boolean useFile) {
        String rawInput
        if (useFile) {
            File file = new File("./day18-input.txt")
            rawInput = file.text
        } else {
            rawInput = """
                2,2,2
                1,2,2
                3,2,2
                2,1,2
                2,3,2
                2,2,1
                2,2,3
                2,2,4
                2,2,6
                1,2,5
                3,2,5
                2,1,5
                2,3,5
                """
        }

        rawInput.trim().split("\n").collect { line ->
            def coords = line.trim().split(",").collect { it.toInteger() }
            new Cube(x: coords[0], y: coords[1], z: coords[2], isLava: true)
        }
    }

    static Cube[][][] initializeSpace(List<Cube> lavaCubes) {
        int maxX = 0
        int maxY = 0
        int maxZ = 0

        lavaCubes.each { Cube lavaCube ->
            if (lavaCube.x > maxX) {
                maxX = lavaCube.x
            }
            if (lavaCube.y > maxY) {
                maxY = lavaCube.y
            }
            if (lavaCube.z > maxZ) {
                maxZ = lavaCube.z
            }
        }

        Cube[][][] space = [ [ [ null ] * (maxZ + 2) ] * (maxY + 2) ] * (maxX + 2)
        space
    }

    static int calculatePart1(Cube[][][] space,
                               List<Cube> lavaCubes) {

        int surfaceArea = 0
        lavaCubes.each { Cube lavaCube ->
            space[lavaCube.x][lavaCube.y][lavaCube.z] = lavaCube
            int neighbours = countLavaNeighbours(space, lavaCube)
            surfaceArea += 6 - 2 * neighbours
        }

        println("Answer 1: ${surfaceArea}") // 3650
        surfaceArea
    }

    static void calculatePart2(Cube[][][] space,
                               List<Cube> lavaCubes) {
        Set<Cube> steamCubes = []
        println("Lava cubes ${lavaCubes.size()}")

        // Create steam from lava cubes
        lavaCubes.each { Cube lavaCube ->
            steamCubes.addAll(generateSteamNeighbours(space, lavaCube))
        }
        println("Initial steam cubes: ${steamCubes.size()}")

        // Propagate steam and repeat N times
        (0..4).each { it ->
            Set newCubes = []
            steamCubes.each { steamCube ->
                newCubes.addAll(generateSteamNeighbours(space, steamCube))
            }
            steamCubes.addAll(newCubes)
            println("Steam cubes: ${steamCubes.size()}")
        }

        // Delete steam cubes again and count contacts with lava cubes
        int surfaceArea = 0
        while (true) {
            println("Steam cubes left: ${steamCubes.size()}")
            int cubesDeleted = 0

            def i = steamCubes.iterator()
            while (i.hasNext()) {
                Cube steamCube = i.next()
                if (hasEmptySpaceNeighbour(space, steamCube)) {
                    surfaceArea += countLavaNeighbours(space, steamCube)
                    cubesDeleted += 1
                    space[steamCube.x][steamCube.y][steamCube.z] = null
                    i.remove()
                }
            }

            println("Cubes deleted: ${cubesDeleted}")
            println("New surface area: ${surfaceArea}")
            if (cubesDeleted == 0) {
                break
            }
        }

        println("Answer 2: ${surfaceArea}") // 2118
    }

    static int countLavaNeighbours(Cube[][][] space, Cube cube) {
        int neighbours = 0

        // X neighbours
        if (cube.x > 0) {
            Cube neighbour = space[cube.x - 1][cube.y][cube.z]
            if (neighbour != null && neighbour.isLava) {
                neighbours++
            }
        }

        if (cube.x < space.length - 1) {
            Cube neighbour = space[cube.x + 1][cube.y][cube.z]
            if (neighbour != null && neighbour.isLava) {
                neighbours++
            }
        }

        // Y neighbours
        if (cube.y > 0) {
            Cube neighbour = space[cube.x][cube.y - 1][cube.z]
            if (neighbour != null && neighbour.isLava) {
                neighbours++
            }
        }
        if (cube.y < space[0].length - 1) {
            Cube neighbour = space[cube.x][cube.y + 1][cube.z]
            if (neighbour != null && neighbour.isLava) {
                neighbours++
            }
        }

        // Z neighbours
        if (cube.z > 0) {
            Cube neighbour = space[cube.x][cube.y][cube.z - 1]
            if (neighbour != null && neighbour.isLava) {
                neighbours++
            }
        }
        if (cube.z < space[0][0].length - 1) {
            Cube neighbour = space[cube.x][cube.y][cube.z + 1]
            if (neighbour != null && neighbour.isLava) {
                neighbours++
            }
        }

        neighbours
    }

    static List<Cube> generateSteamNeighbours(Cube[][][] space,
                                              Cube cube) {
        List<Cube> steamCubes = []

        // X neighbours
        if (cube.x > 0 && space[cube.x - 1][cube.y][cube.z] == null) {
            def steamCube = new Cube(x: cube.x - 1, y: cube.y, z: cube.z, isLava: false)
            steamCubes << steamCube
            space[steamCube.x][steamCube.y][steamCube.z] = steamCube
        }
        if (cube.x < space.length - 1 && space[cube.x + 1][cube.y][cube.z] == null) {
            def steamCube = new Cube(x: cube.x + 1, y: cube.y, z: cube.z, isLava: false)
            steamCubes << steamCube
            space[steamCube.x][steamCube.y][steamCube.z] = steamCube
        }

        // Y neighbours
        if (cube.y > 0 && space[cube.x][cube.y - 1][cube.z] == null) {
            def steamCube = new Cube(x: cube.x, y: cube.y - 1, z: cube.z, isLava: false)
            steamCubes << steamCube
            space[steamCube.x][steamCube.y][steamCube.z] = steamCube
        }
        if (cube.y < space[0].length - 1 && space[cube.x][cube.y + 1][cube.z] == null) {
            def steamCube = new Cube(x: cube.x, y: cube.y + 1, z: cube.z, isLava: false)
            steamCubes << steamCube
            space[steamCube.x][steamCube.y][steamCube.z] = steamCube
        }

        // Z neighbours
        if (cube.z > 0 && space[cube.x][cube.y][cube.z - 1] == null) {
            def steamCube = new Cube(x: cube.x, y: cube.y, z: cube.z - 1, isLava: false)
            steamCubes << steamCube
            space[steamCube.x][steamCube.y][steamCube.z] = steamCube
        }
        if (cube.z < space[0][0].length - 1 && space[cube.x][cube.y][cube.z + 1] == null) {
            def steamCube = new Cube(x: cube.x, y: cube.y, z: cube.z + 1, isLava: false)
            steamCubes << steamCube
            space[steamCube.x][steamCube.y][steamCube.z] = steamCube
        }

        steamCubes
    }

    static boolean hasEmptySpaceNeighbour(Cube[][][] space,
                                          Cube cube) {

        // X neighbours
        if (cube.x == 0 || space[cube.x - 1][cube.y][cube.z] == null) {
            return true
        }

        if (cube.x == space.length - 1 || space[cube.x + 1][cube.y][cube.z] == null) {
            return true
        }

        // Y neighbours
        if (cube.y == 0 || space[cube.x][cube.y - 1][cube.z] == null) {
            return true
        }
        if (cube.y == space[0].length - 1 || space[cube.x][cube.y + 1][cube.z] == null) {
            return true
        }

        // Z neighbours
        if (cube.z == 0 || space[cube.x][cube.y][cube.z - 1] == null) {
            return true
        }
        if (cube.z == space[0][0].length - 1 || space[cube.x][cube.y][cube.z + 1] == null) {
            return true
        }

        return false
    }
}

Day18.main();
