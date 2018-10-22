open TestReconciler;

let test = (msg, t) => {
    print_endline ("[TEST] Begin " ++ msg);
    t();
    print_endline ("[TeST] End" ++ msg);
};

let assertIntEqual = (actual, expected, msg) => {
    if (actual == expected) {
        print_endline("[PASS] " ++ msg ++ "[expected: " ++ string_of_int(expected) ++ ", actual: " ++ string_of_int(actual) ++ "]");
    } else {
        print_endline("[FAIL] " ++ msg ++ "[expected: " ++ string_of_int(expected) ++ ", actual: " ++ string_of_int(actual) ++ "]");
    }
    assert(actual == expected);
};

type tree('a) =
    | TreeNode('a, list(tree('a)))
    | TreeLeaf('a);

let validateStructure = (rootNode: node , structure: tree(primitives)) => {
    let rec f = (inputNode: node, st: tree(primitives), level) => {
        switch (st) {
        | TreeNode(p, c) => {
            assert(inputNode.nodeType == p);
            assertIntEqual(List.length(inputNode.children^), List.length(c), "Validating children tree at level: " ++ string_of_int(level));

            List.iter2((a, b) => f(a, b, level + 1), inputNode.children^, c)
        }
        | TreeLeaf(p) => {
            assert(inputNode.nodeType == p);
        };
    };
    };

    f(rootNode, structure, 0);
};
