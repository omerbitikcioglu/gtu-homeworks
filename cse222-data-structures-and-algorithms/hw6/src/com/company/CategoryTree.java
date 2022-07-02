package com.company;

import com.sun.source.tree.BinaryTree;

import java.io.Serializable;

/**
 * Category tree holds the information of categories
 * Same level categories goes beyond to the left children
 * Descent categories goes beyond to the right children
 *
 * @author Ömer Faruk Bitikçioğlu
 */
public class CategoryTree {
    private Node root;

    // Constructors
    /** No-parameter constructor for CategoryTree */
    public CategoryTree() {
        root = null;
    }

    /**
     * Encapsulates nodes of the category tree
     * Each node represents a category of the hierarchy
     */
    protected static class Node {
        protected String data;
        protected int depth;
        protected Node left;
        protected Node right;

        public Node(String data, int depth) {
            this.data = data;
            this.depth = depth;
            this.left = null;
            this.right = null;
        }
    }

    /**
     * Adds a new category if not exists
     * @param categoryName The name of the category to be added
     * @param depth The depth of the category to be added
     */
    public void addCategory(String categoryName, int depth) {
        root = addCategory(root, categoryName, depth);
    }

    /**
     * Adds a new category node to the category tree.
     * If the category node exists, it does nothing.
     * pre: depth must be 0 at the beginning
     * @param root the root of the tree
     * @param categoryName name of the category
     * @param depth depth of the category
     * @return the newly added category node
     */
    private Node addCategory(Node root, String categoryName, int depth) {
        if (root == null) {
            return new Node(categoryName, depth);
        } else if (root.data.equals(categoryName)) {
            return root;
        } else if (root.depth == depth) { // Same depths goes beyond left children
            root.left = addCategory(root.left, categoryName, depth);
            return root;
        } else { // Greater depths goes beyond right children
            root.right = addCategory(root.right, categoryName, depth);
            return root;
        }
    }

//    /**
//     * Creates the string of category hierarchy through the given category
//     * @param categoryName The name of the category at the last node
//     * @return the created string of hierarchy
//     */
//    public String categoryHierarchy(String categoryName) {
//        StringBuilder sb = new StringBuilder();
//        sb.append("\"[\"\"");
//
//        while (root.data != categoryName) {
//            sb.append(root.data);
//            sb.append(" >> ");
//            if (root.depth == depth) {
//                root = root.left;
//            } else {
//                root = root.right;
//            }
//        }
//        sb.append("\"\"]\"");
//        return sb.toString();
//    }
}
