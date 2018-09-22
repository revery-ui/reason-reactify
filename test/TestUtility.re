open TestReconciler;

type tree('a) =
    | TreeNode('a, list(tree('a)))
    | TreeLeaf('a);

let validateStructure = (rootNode: node , structure: tree(primitives)) => {
    let rec f = (inputNode: node, st: tree(primitives), level) => {
        switch (st) {
        | TreeNode(p, c) => {
            assert(inputNode.nodeType == p);
            assert(List.length(inputNode.children^) == List.length(c));

            List.iter2((a, b) => f(a, b, level + 1), inputNode.children^, c)
        }
        | TreeLeaf(p) => {
            assert(inputNode.nodeType == p);
        };
    };
    };

    f(rootNode, structure, 0);
};
