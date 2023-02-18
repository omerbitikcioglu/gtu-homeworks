package com.example;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.security.InvalidParameterException;
import java.sql.Array;
import java.util.Arrays;

public class Main {
    private static final int MAX_DEFECT = 4;
    private static final int TIME = 20;
    private static final int NUM_OF_COMPANIES = 14;

    public static void main(String[] args) {
        // (a)
        int[] cases = giveCases();
        System.out.printf("%5s %20s", "# of", "# of cases");
        System.out.println();
        System.out.printf("%5s %20s", "Defects", "in all company");
        System.out.println();
        System.out.printf("%5s %20s", "\t\t", "between the years");
        System.out.println();
        for (int i = 0; i <= MAX_DEFECT; ++i) {
            System.out.format("%5d %15d", i, cases[i]);
            System.out.println();
        }
        System.out.println();

        // (b)
	    double lambda = estimateLambda();
        System.out.printf("Lambda = %.1f\n\n", lambda);

        // (c)
        int totalCase = Arrays.stream(cases).sum();
        double[] predictions = findPredictions(totalCase, lambda);
        System.out.printf("%5s %20s %35s", "# of", "# of cases", "Predicted n# of cases");
        System.out.println();
        System.out.printf("%5s %20s %30s", "Defects", "in all company", "in all companies");
        System.out.println();
        System.out.printf("%5s %20s %30s", "\t\t", "between the years", "between the years");
        System.out.println();
        for (int i = 0; i <= MAX_DEFECT; ++i) {
            System.out.format("%5d %15d %35f", i, cases[i], predictions[i]);
            System.out.println();
        }
    }

    /**
     * Calculates and returns the predicted case values.
     * Uses Poisson distribution to calculate the predictions.
     * @param totalCase the number of total cases for all the time
     * @param lambda the lambda value
     * @return the predicted case values
     */
    private static double[] findPredictions(int totalCase, double lambda) {
        double[] predictions = new double[MAX_DEFECT+1];
        double negLambda = (-1) * lambda;
        for (int i = 0; i <= MAX_DEFECT; ++i) {
            predictions[i] = ((Math.exp(negLambda) * Math.pow(lambda, i)) / fact(i)) * totalCase;
        }
        return predictions;
    }

    /**
     * Find the factorial of given number
     * @param num the number to be processed
     * @return the factorial of the given number
     */
    private static double fact(int num) {
        if (num < 0) {
            throw new InvalidParameterException();
        }
        int result = 1;
        for (int mult = 2 ; mult <= num; ++mult) {
            result = result * mult;
        }
        return result;
    }

    /**
     * Calculates the lambda according to the given data
     * @return the lambda value
     */
    private static double estimateLambda() {
        double lambda = 0.0;
        try {
            BufferedReader br = new BufferedReader(new FileReader("manufacturing_defects.txt"));
            String line = br.readLine();
            while (line != null) {
                String[] row = line.split("\t");
                double total = 0.0;
                for (int i = 2; i < row.length; ++i) {
                    total += Integer.parseInt(row[i]);
                }
                lambda += total / NUM_OF_COMPANIES; // Calculate the mean for each year
                line = br.readLine();
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        lambda /= TIME; // Calculate the mean for all years
        return lambda;
    }

    /**
     * Calculates and returns the total number of actual cases
     * for each defect.
     * @return the number of actual cases for each defect
     */
    private static int[] giveCases() {
        try {
            BufferedReader br = new BufferedReader(new FileReader("manufacturing_defects.txt"));
            String line = br.readLine();
            int[] cases = new int[MAX_DEFECT+1];
            while (line != null) {
                String[] row = line.split("\t");
                // Count the number of cases for all companies and all years
                // for each number of defects (# of defects -> 0,1,2,3,4)
                for (int i = 2; i < row.length; ++i) {
                    int index = Integer.parseInt(row[i]); // 0,1,2,3 or 4
                    cases[index]++;
                }
                line = br.readLine();
            }
            br.close();
            return cases;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
