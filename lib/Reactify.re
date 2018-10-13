/*
 * Interface to bridge a React-style functional API
 * with a mutable back-end. Similiar in spirit
 * to the reconciler interface provided via 'react-conciler'.
 */
module type Reconciler = {

    /* 
       Primitives is a variant type describing metadata needed
       to instantiate something in the mutable world.
    */
    type primitives;

    /*
       A node is a live instance representing a mutable object
    */
    type node;

  let appendChild: (node, node) => unit;

  let createInstance: primitives => node;

  let replaceChild: (node, node, node) => unit;

  let removeChild: (node, node) => unit;

  let updateInstance: unit /* TODO: (node, primitives) */ => unit;
};

module Make = (ReconcilerImpl: Reconciler) => {
  type element =
    | Primitive(ReconcilerImpl.primitives)
    | Component
  and renderedElement =
    | RenderedPrimitive(ReconcilerImpl.node)
  and elementWithChildren = (element, childComponents)
  and component = {render: unit => elementWithChildren}
  and childComponents = list(component)
  and instance = {
    component,
    element,
    node: option(ReconcilerImpl.node),
    childInstances,
  }
  and childInstances = list(instance);

  type container = {
       rootInstance: ref(option(instance)),
       rootNode: ReconcilerImpl.node,
  };

  let createContainer = (rootNode: ReconcilerImpl.node) => {
       let ret: container = {
            rootNode,
            rootInstance: ref(None)
        };
      ret;
  };

  type wrappedComponent = {
        render: unit => component,
  };

  let statelessComponent = (c: wrappedComponent, ~children: childComponents) => {
        let ret: component = {
            render: () => {
                let children: list(component) = [c.render()];
                let renderResult: elementWithChildren = (Component, children);
                renderResult;
           },
       };
       ret;
  };

  let primitiveComponent = (prim, ~children) => {
    let comp: component = {
      render: () => {
        print_endline(
          "Rendering primitive! Children length: "
          ++ string_of_int(List.length(children)),
        );
        (Primitive(prim), children);
      },
    };
    comp;
  };

  let rec instantiate = (rootNode, component: component) => {
    let (element, children) = component.render();

    let primitiveInstance =
        switch (element) {
        | Primitive(p) => Some(ReconcilerImpl.createInstance(p))
        | _ => None
        };

    let nextRootPrimitiveInstance =
      switch (primitiveInstance) {
      | Some(i) => i
      | None => rootNode
      };

    let childInstances = List.map(instantiate(nextRootPrimitiveInstance), children);

    let appendIfInstance = (ci) => {
        switch (ci.node) {
        | Some(s) => {
                print_endline("-appendIfIsntance: appending!");
                ReconcilerImpl.appendChild(nextRootPrimitiveInstance, s)
        }
        | _ => ()
        }
    };
  
    List.iter(appendIfInstance, childInstances);

    let instance = {
       component,
       element,
       node: primitiveInstance,
       childInstances
    }

    instance
  };

  let reconcile = (rootNode, instance, component) => {
      print_endline("start reconcile");
      let r = switch (instance) {
        | None => {
            let newInstance = instantiate(rootNode, component);
            print_endline(" - new ROOT instance!");

            switch(newInstance.node) {
            | Some(n) => ReconcilerImpl.appendChild(rootNode, n)
            | None => ()
            };

            newInstance
        }
        | Some(i) => {
            let newInstance = instantiate(rootNode, component);
            print_endline(" - replace ROOT instance!");

            switch((newInstance.node, i.node)) {
            | (Some(a), Some(b)) => ReconcilerImpl.replaceChild(rootNode, a, b)
            | (Some(a), None) => ReconcilerImpl.appendChild(rootNode, a)
            | (None, Some(b)) => ReconcilerImpl.removeChild(rootNode, b)
            | (None, None) => ()
            };

            newInstance
        }
      };
      print_endline("end reconcile");
      r;
  };

  let updateContainer = (container, component) => {
    let {rootNode, rootInstance} = container;
    let prevInstance = rootInstance^;
    let nextInstance = reconcile(rootNode, prevInstance, component);
    rootInstance := Some(nextInstance);
  };
};
