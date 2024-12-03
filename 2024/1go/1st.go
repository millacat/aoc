package main

import (
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

func readFile(path string) []byte {
	content, err := os.ReadFile(path)
	if err != nil {
		fmt.Println("Error reading file", err)
	}
	return content
}

func getColumns(lines []string, col0 []int, col1 []int) ([]int, []int) {
	for i := 0; i < len(lines); i++ {
		line := strings.Fields(lines[i])
		val0, _ := strconv.Atoi(line[0])
		val1, _ := strconv.Atoi(line[1])
		col0 = append(col0, val0)
		col1 = append(col1, val1)
	}
	return col0, col1
}

func sortSlice(slice []int) {
	sort.Slice(slice, func(i, j int) bool {
		return slice[i] < slice[j]
	})
}

func calculateDistance(col0, col1 []int) int {
	var distance float64
	for i := 0; i < len(col0); i++ {
		distance += math.Abs(float64(col0[i] - col1[i]))
	}

	return int(distance)
}

func main() {
	data := readFile("../1/input")
	lines := strings.Split(string(data), "\n")

	var col0, col1 []int
	col0, col1 = getColumns(lines, col0, col1)

	sortSlice(col0)
	sortSlice(col1)

	distance := calculateDistance(col0, col1)

	// Calculate similarity score: https://adventofcode.com/2024/day/1

	fmt.Println("Day 1, first result:", distance)
}
