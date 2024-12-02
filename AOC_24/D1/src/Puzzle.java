/*
This program will take input of two lists of numbers (5 digits) separated by three spaces.
Example input: 1 2 3 4 5   6 7 8 9 10.
Program should find the smallest number in the first list and the smallest number in the second list
and then subtract them (and do this with the entire input recursively). The result of each will be stored in a new list.
The program will then add the numbers in the new list and output the result.
 */


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class Puzzle {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Enter the left and right lists (format: left   right):");
        System.out.println("Type 'q' to calculate and quit.");

        ArrayList<String> inputLines = new ArrayList<>();

        // Read input lines until 'q' is entered
        while (true) {
            String input = scanner.nextLine().trim();
            if (input.equalsIgnoreCase("q")) {
                break;
            }
            inputLines.add(input);
        }

        int totalDistance = 0;

        try {
            // Collect all left and right numbers into separate lists
            ArrayList<Integer> leftList = new ArrayList<>();
            ArrayList<Integer> rightList = new ArrayList<>();

            for (String line : inputLines) {
                // Split the line into left and right numbers
                String[] parts = line.split("   "); // Split by three spaces
                if (parts.length != 2) {
                    throw new IllegalArgumentException("Each line must contain two numbers separated by three spaces.");
                }

                // Parse the left and right numbers
                leftList.add(Integer.parseInt(parts[0].trim()));
                rightList.add(Integer.parseInt(parts[1].trim()));
            }

            // Sort both lists
            int[] sortedLeft = leftList.stream().mapToInt(i -> i).sorted().toArray();
            int[] sortedRight = rightList.stream().mapToInt(i -> i).sorted().toArray();

            // Calculate the total distance
            for (int i = 0; i < sortedLeft.length; i++) {
                int diff = Math.abs(sortedLeft[i] - sortedRight[i]);
                totalDistance += diff;
                System.out.println("Pair: (" + sortedLeft[i] + ", " + sortedRight[i] + "), Distance: " + diff);
            }

            // Output the total distance
            System.out.println("The total distance is: " + totalDistance);

        } catch (IllegalArgumentException e) {
            System.out.println(e.getMessage());
        } finally {
            scanner.close();
        }
    }
}