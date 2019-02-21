type state = {
  show: bool,
  login: string,
  password: string,
};

type action =
  | Password(string)
  | Login(string)
  | OnLogin;

let onChange = (send, f, e) => e->ReactEvent.Form.target##value->f->send;

let make = _children => {
  ...ReasonReact.reducerComponent("Login"),
  initialState: () => {show: true, login: "", password: ""},
  reducer: (action, state) =>
    switch (action) {
    | Password(password) => ReasonReact.Update({...state, password})
    | Login(login) => ReasonReact.Update({...state, login})
    | OnLogin => ReasonReact.Update({...state, show: false})
    },
  render: ({state, send}) =>
    <div className={state.show ? "modal is-active" : "modal"}>
      <form
        onSubmit={e => {
          ReactEvent.Form.preventDefault(e);
          send(OnLogin);
        }}>
        <div className="modal-background" />
        <div className="modal-card">
          <header className="modal-card-head">
            <p className="modal-card-title">
              {ReasonReact.string("Chuck App Login")}
            </p>
          </header>
          <section className="modal-card-body">
            <div className="field">
              <label className="label">
                {ReasonReact.string("Username")}
              </label>
              <div className="control">
                <input
                  className="input"
                  type_="text"
                  placeholder="john123"
                  value={state.login}
                  onChange={onChange(send, v => Login(v))}
                />
              </div>
            </div>
            <div className="field">
              <label className="label">
                {ReasonReact.string("Password")}
              </label>
              <div className="control">
                <input
                  className="input"
                  type_="password"
                  placeholder="your password"
                  value={state.password}
                  onChange={onChange(send, v => Password(v))}
                />
              </div>
            </div>
          </section>
          <footer className="modal-card-foot">
            <button
              className="button is-success"
              disabled={
                !Password.validate_pass(state.password)
                || String.length(state.login) == 0
              }>
              {ReasonReact.string("Login")}
            </button>
          </footer>
        </div>
      </form>
    </div>,
};