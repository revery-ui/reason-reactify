module type Reconciler = {
    type primitives;

    type node;

    let appendChild: (node, node) => unit;

    let createInstance: (primitives) => node;

    let removeChild: (node, node) => unit;

    let updateInstance: (node, primitives) => unit;
};

module Make = (ReconcilerImpl : Reconciler) => {
    type element =
        | Primitive(ReconcilerImpl.primitives)
    and renderedElement =
        | RenderedPrimitive(ReconcilerImpl.node)
    and elementWithChildren = (element, childComponents)
    and component = {
        render: unit => elementWithChildren
    }
    and childComponents = list(component)
    and instance ={
        component,
        element,
        renderedElement,
        childInstances
    }
    and childInstances = list(instance)

    let primitiveComponent = (prim, ~children) => {
        let comp: component = {
            render: () => {
                    print_endline("Rendering primitive! Children length: " ++ string_of_int(List.length(children)));
                    (Primitive(prim), children)
            }
        };
        comp;
    };

    let rec render = (rootNode: ReconcilerImpl.node, input) => {
        let (element, children) = input.render();

        let primitiveInstance = switch (element) {
        | Primitive(p) => Some(ReconcilerImpl.createInstance(p))
        };

        let nextRootPrimitiveInstance = switch(primitiveInstance) {
        | Some(i) => i
        | None => rootNode
        }

        let renderChild = (i: component) => render(nextRootPrimitiveInstance, i);

        List.iter(renderChild, children)

        switch (primitiveInstance) {
        | Some(p) => ReconcilerImpl.appendChild(rootNode, p);
        | _ => ()
        };

        print_endline ("Render called!");
    };
};
