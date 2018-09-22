/** Simple test cases */

open TestReconciler;

module TestReact = Reactify.Make(TestReconciler);

let rootNode: TestReconciler.node = {
    children: ref([]),
    nodeType: Root
};

let aComponent = (~testVal, ~children, ()) => TestReact.primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) => TestReact.primitiveComponent(B, ~children);
let cComponent = (~children, ()) => TestReact.primitiveComponent(C, ~children);

let component = () => {
    <aComponent testVal={1}>
        <bComponent />
        <bComponent>
            <cComponent />
        </bComponent>
    </aComponent>
}

TestReact.render(rootNode, component());

open TestReconciler;

type tree('a) =
    | TreeNode('a, list(tree('a)))
    | TreeLeaf('a);

let expectedStructure: tree(primitives) = TreeNode(Root, 
                             [TreeNode(
        A(1), [
         TreeLeaf(B),
         TreeNode(B, [TreeLeaf(C)])
        ])
                             ]
);

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

validateStructure(rootNode, expectedStructure);

TestReact.render(rootNode, component());

/* TODO: validateStructure(rootNode, expectedStructure); */
