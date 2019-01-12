/** Simple test cases */
open Rejest;

open Brisk_core;

type noop = unit => unit;

type primitives =
| Root
| A(int)
| B
| C(string)
| D(noop)


module Node {
    let id = ref(0);
    type t = {
      children: ref(list(t)),
      mutable nodeType: primitives,
      nodeId: int,
    };

    let create = (prim: primitives) => {
        let ret: t = {
            children: ref([]),
            nodeType: prim,
            nodeId: id^,
        };

        id := id^ + 1;
        ret;
    };
};

module TestRenderer = {
    type hostElement = Node.t;
    type node = Node.t;

    let insertNode = (~parent: node, ~child: node, ~position as _) => {
        parent.children := parent.children^ @ [child];
        parent
    };

    let markAsStale = () => ();

    let deleteNode = (~parent: Node.t, ~child: Node.t) => {
        parent.children := List.filter(c => c != child, parent.children^);
        parent
    };

   let beginChanges = () => ();
   let commitChanges = () => ();

   let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => parent;
};

module React = ReactCore.Make(TestRenderer);

module AComponent {
    let component = React.nativeComponent("view");
    let make = (~testVal: int, children) => {
        component((_: Slots.empty) => {
            make: () => {
                let node = Node.create(A(testVal));
                node;
            },
            configureInstance: (~isFirstRender as _, node: Node.t) => {
                node.nodeType = A(testVal);    
                node;
            },
            children,
        });
    };

    let createElement = (~testVal:int, ~children, ()) =>
        React.element(make(~testVal, React.listToElement(children)));

};

type tree('a) =
  | TreeNode('a, list(tree('a)))
  | TreeLeaf('a);

let validateStructure = (rootNode: Node.t, structure: tree(primitives)) => {
  let rec f = (inputNode: Node.t, st: tree(primitives), level) =>
    switch (st) {
    | TreeNode(p, c) =>
      expect(inputNode.nodeType).toEqual(p);
      expect(List.length(inputNode.children^)).toEqual(List.length(c));

      List.iter2((a, b) => f(a, b, level + 1), inputNode.children^, c);
    | TreeLeaf(p) => expect(inputNode.nodeType).toEqual(p)
    };

  f(rootNode, structure, 0);
};

test("BRISK_PrimitiveComponent", () => {
  test("rendering a primitive", () => {

      let root = Node.create(Root);
      let rendered = React.RenderedElement.render(root, <AComponent testVal={1} />);
      React.RenderedElement.executeHostViewUpdates(rendered) |> ignore;

      expect(List.length(root.children^)).toEqual(1);

      validateStructure(root, TreeNode(Root, [TreeLeaf(A(1))]));
  });

  test("rendering a test component", () => {

      let component = React.component("Other");
      let customComponent = (~testVal: int, ~children as _, ()) => React.element(component(_slots => {
        <AComponent testVal >
            <AComponent testVal={2} />
        </AComponent>
      }));

      let root = Node.create(Root);
      let rendered = React.RenderedElement.render(root, <customComponent testVal={1} />);
      React.RenderedElement.executeHostViewUpdates(rendered) |> ignore;

      expect(List.length(root.children^)).toEqual(1);
      validateStructure(root, TreeNode(Root, 
                                       [
                                         TreeNode(A(1),
                                                  [TreeLeaf(A(2))])]));
  });
});
