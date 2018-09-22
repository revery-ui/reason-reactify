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

  let rootInstance: ref(option(instance)) = ref(None);

  let rec instantiate = (rootNode, component) => {
    let (element, children) = component.render(); 

    let primitiveInstance =
        switch (element) {
        | Primitive(p) => Some(ReconcilerImpl.createInstance(p))
        };

    let nextRootPrimitiveInstance =
      switch (primitiveInstance) {
      | Some(i) => i
      | None => rootNode
      };    
    
    let childInstances = List.map(instantiate(nextRootPrimitiveInstance), children);

    let instance = {
       component,
       element,
       node: primitiveInstance,
       childInstances
    }

    instance
  };

  let reconcile = (rootNode, instance, component) => {
      switch (instance) {
        | None => {
            let newInstance = instantiate(rootNode, component);

            switch(newInstance.node) {
            | Some(n) => ReconcilerImpl.appendChild(rootNode, n)
            | None => ()
            };

            newInstance
        }
        | Some(i) => {
            let newInstance = instantiate(rootNode, component);     

            switch((newInstance.node, i.node)) {
            | (Some(a), Some(b)) => ReconcilerImpl.replaceChild(rootNode, a, b)
            | (Some(a), None) => ReconcilerImpl.appendChild(rootNode, a)
            | (None, Some(b)) => ReconcilerImpl.removeChild(rootNode, b)
            | (None, None) => ()
            };

            newInstance
        }
      };
  };

  let render = (rootNode: ReconcilerImpl.node, component) => {
      let prevInstance = rootInstance^;
      let nextInstance = reconcile(rootNode, prevInstance, component);
      rootInstance := Some(nextInstance);

  };

      /*
    let (element, children) = input.render();

    let primitiveInstance =
      switch (element) {
      | Primitive(p) => Some(ReconcilerImpl.createInstance(p))
      };

    let nextRootPrimitiveInstance =
      switch (primitiveInstance) {
      | Some(i) => i
      | None => rootNode
      };

    let renderChild = (i: component) => render(nextRootPrimitiveInstance, i);

    List.iter(renderChild, children);

    switch (primitiveInstance) {
    | Some(p) => ReconcilerImpl.appendChild(rootNode, p)
    | _ => ()
    };

    print_endline("Render called!"); */
};
