let make = _children => {
  ...ReasonReact.statelessComponent("App"),
  render: _self =>
    <div className="section">
      <div className="container"> <Jokes /> </div>
    </div>,
};