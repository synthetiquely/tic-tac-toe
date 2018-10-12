type item = {
  id: int,
  title: string,
  completed: bool,
};

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~item, ~onToggle, _children) => {
    ...component,
    render: _self =>
      <div className="item" onClick={_event => onToggle()}>
        <input type_="checkbox" checked={item.completed} />
        {ReasonReact.string(item.title)}
      </div>,
  };
};

module TextInput = {
  type state = {value: string};
  type action =
    | UpdateForm(string);

  let component = ReasonReact.reducerComponent("TextInput");

  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => {value: ""},
    reducer: (action, state) =>
      switch (action) {
      | UpdateForm(text) => ReasonReact.Update({...state, value: text})
      },
    render: self =>
      <input
        type_="text"
        value={self.state.value}
        placeholder="Write something to do"
        onChange={
          event =>
            self.send(UpdateForm(ReactEvent.Form.target(event)##value))
        }
        onKeyDown={
          event =>
            if (ReactEvent.Keyboard.key(event) === "Enter") {
              onSubmit(self.state.value);
              self.send(() => "");
            }
        }
      />,
  };
};

type action =
  | AddItem(string)
  | ToggleItem(int);

type state = {items: list(item)};

let lastId = ref(0);
let newItem = text => {
  lastId := lastId^ + 1;
  {id: lastId^, title: text, completed: false};
};

let component = ReasonReact.reducerComponent("App");

let make = _children => {
  ...component,
  initialState: () => {
    items: [{id: 0, title: "Write some things to do", completed: false}],
  },
  reducer: (action, {items}) =>
    switch (action) {
    | AddItem(text) =>
      ReasonReact.Update({items: [newItem(text), ...items]})
    | ToggleItem(id) =>
      let items =
        List.map(
          item =>
            item.id === id ? {...item, completed: !item.completed} : item,
          items,
        );
      ReasonReact.Update({items: items});
    },
  render: ({state: {items}, send}) => {
    let numItems = List.length(items);
    <div className="app">
      <div className="title"> {ReasonReact.string("What to do")} </div>
      <TextInput onSubmit={text => send(AddItem(text))} />
      <div className="items">
        {
          List.map(
            item =>
              <TodoItem
                key={string_of_int(item.id)}
                item
                onToggle={() => send(ToggleItem(item.id))}
              />,
            items,
          )
          |> Array.of_list
          |> ReasonReact.array
        }
      </div>
      <div className="footer">
        {ReasonReact.string(string_of_int(numItems) ++ " items")}
      </div>
    </div>;
  },
};
