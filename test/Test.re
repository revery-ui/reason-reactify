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
    <bComponent />
  </aComponent>;

let test = (msg, t) => {
    print_endline ("[TEST] Begin " ++ msg);
    t();
    print_endline ("[TeST] End" ++ msg);
};

test("BasicRenderTest", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  TestReact.updateContainer(container, <bComponent />);

  let expectedStructure = TreeNode(Root, [TreeLeaf(B)]);
  validateStructure(rootNode, expectedStructure);
});

test("UpdateNodeTest", () => {
    let rootNode = createRootNode();
    let container = TestReact.createContainer(rootNode);

    TestReact.updateContainer(container, <aComponent testVal={1}/>);

    let expectedStructure = TreeNode(Root, [TreeLeaf(A(1))]);
    validateStructure(rootNode, expectedStructure);

    /* Now, we'll update the tree */
    TestReact.updateContainer(container, <aComponent testVal={2}/>);

    let expectedStructure = TreeNode(Root, [TreeLeaf(A(2))]);
    validateStructure(rootNode, expectedStructure);
});

test("ReplaceNodeTest", () => {
   let rootNode = createRootNode();
   let container = TestReact.createContainer(rootNode);

   TestReact.updateContainer(container, <aComponent testVal={1}/>);

   let expectedStructure = TreeNode(Root, [TreeLeaf(A(1))]);
   validateStructure(rootNode, expectedStructure);

   /* Now, we'll update the tree */
   print_endline ("going for the update....");
   TestReact.updateContainer(container, <bComponent/>);

   let expectedStructure = TreeNode(Root, [TreeLeaf(B)]);
   validateStructure(rootNode, expectedStructure);
});


test("RenderingChildrenTest", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeNode(A(1), [TreeLeaf(B), TreeLeaf(B)])]
    );

  TestReact.updateContainer(container, component());

  validateStructure(rootNode, expectedStructure);
});

let componentWrappingB = (~children, ()) => TestReact.statelessComponent({
     render: () => <bComponent />
}, ~children);

test("Rendering simple wrapped component", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(B)],
    );

  TestReact.updateContainer(container, <componentWrappingB />);

  validateStructure(rootNode, expectedStructure);
});


let componentWrappingAWithProps = (~children, ~v, ()) => 
        TestReact.statelessComponent({
             render: () => <aComponent testVal=v />
        }, ~children);


test("Rendering wrapped component with prop", () => {
  let rootNode = createRootNode();
  let container = TestReact.createContainer(rootNode);

  let expectedStructure: tree(primitives) =
    TreeNode(
      Root,
      [TreeLeaf(A(7))],
    );

  TestReact.updateContainer(container, <componentWrappingAWithProps v=7 />);

  validateStructure(rootNode, expectedStructure);
});


/* let componentWithWrappedComponents = TestReact.statelessComponent({ */
/*       render: () => <aComponent testVal=1> */
/*         <componentWrappingB /> */
/*       </aComponent> */
/* }); */

/* TODO: validateStructure(rootNode, expectedStructure); */
