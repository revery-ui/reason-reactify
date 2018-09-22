/** Simple test cases */
open TestReconciler;
open TestUtility;

/* Use our Reconciler to create our own instance */
module TestReact = Reactify.Make(TestReconciler);

let createRootNode = () => {children: ref([]), nodeType: Root};

let aComponent = (~testVal, ~children, ()) =>
  TestReact.primitiveComponent(A(testVal), ~children);
let bComponent = (~children, ()) =>
  TestReact.primitiveComponent(B, ~children);
let cComponent = (~children, ()) =>
  TestReact.primitiveComponent(C, ~children);

let component = () =>
  <aComponent testVal=1>
    <bComponent />
    <bComponent> <cComponent /> </bComponent>
  </aComponent>;

module BasicRenderTest {
   let rootNode = createRootNode(); 

   TestReact.render(rootNode, <bComponent />);

   let expectedStructure = TreeNode(Root, [TreeLeaf(B)]);
   validateStructure(rootNode, expectedStructure);
}


/*
module UpdateNodeTest {
   let rootNode = createRootNode(); 

   TestReact.render(rootNode, <aComponent testVal={1}/>);

   let expectedStructure = TreeNode(Root, [TreeLeaf(A(1))]);
   validateStructure(rootNode, expectedStructure);

   /* Now, we'll update the tree */
   TestReact.render(rootNode, <aComponent testVal={2}/>);

   let expectedStructure = TreeNode(Root, [TreeLeaf(A(2))]);
   validateStructure(rootNode, expectedStructure);
}
*/

module RenderingChildrenTest {
    let rootNode = createRootNode();

    let expectedStructure: tree(primitives) =
      TreeNode(
        Root,
        [TreeNode(A(1), [TreeLeaf(B), TreeNode(B, [TreeLeaf(C)])])],
      );

    TestReact.render(rootNode, component());

    validateStructure(rootNode, expectedStructure);
}

/* TODO: validateStructure(rootNode, expectedStructure); */
