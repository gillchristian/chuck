open Data;

type favorites = IntMap.t(Service.joke);
type jokes = list(Service.joke);

type state = {
  data: RemoteData.t(jokes),
  favorites,
};

type action =
  | FetchJokes
  | FetchJokesDone(RemoteData.t(jokes))
  | MarkFavorite(Service.joke)
  | RemoveFavorite(Service.joke);

let make = _children => {
  ...ReasonReact.reducerComponent("Jokes"),
  initialState: () => {data: RemoteData.NotAsked, favorites: IntMap.empty},
  reducer: (action, state) =>
    switch (action) {
    | FetchJokes =>
      ReasonReact.UpdateWithSideEffects(
        {...state, data: RemoteData.Loading},
        self =>
          Service.fetch_jokes(10)
          |> Js.Promise.then_(res =>
               (
                 switch (res) {
                 | Service.Ok(d) => RemoteData.Success(d.Service.value)
                 | Service.Fail(e) => RemoteData.Failure(e)
                 }
               )
               |> Js.Promise.resolve
             )
          |> Js.Promise.then_(data =>
               FetchJokesDone(data) |> self.send |> Js.Promise.resolve
             )
          |> ignore,
      )
    /* TODO: merge with favorites */
    | FetchJokesDone(data) => ReasonReact.Update({...state, data})
    | MarkFavorite(joke) =>
      IntMap.cardinal(state.favorites) >= 10 ?
        ReasonReact.NoUpdate :
        ReasonReact.Update({
          data:
            RemoteData.map(
              js => List.filter(j => joke.id !== j.Service.id, js),
              state.data,
            ),
          favorites: IntMap.add(joke.id, joke, state.favorites),
        })
    | RemoveFavorite(joke) =>
      IntMap.mem(joke.Service.id, state.favorites) ?
        ReasonReact.Update({
          data: RemoteData.map(js => [joke, ...js], state.data),
          favorites: IntMap.remove(joke.id, state.favorites),
        }) :
        ReasonReact.NoUpdate
    },
  render: ({send, state}) =>
    <div>
      <div className="section">
        <button
          className={
            RemoteData.is_loading(state.data) ?
              "button is-primary is-loading" : "button is-primary"
          }
          onClick={_ => send(FetchJokes)}
          disabled={RemoteData.is_loading(state.data)}>
          {ReasonReact.string("I want Chuck Norris Jokes!")}
        </button>
      </div>
      <div className="section">
        {state.favorites
         |> IntMap.values
         |> List.map(j =>
              <Joke
                joke=j
                handleClick={joke => send(RemoveFavorite(joke))}
                isFavorite=true
              />
            )
         |> Array.of_list
         |> ReasonReact.array}
        {switch (state.data) {
         | RemoteData.NotAsked =>
           <div> {ReasonReact.string("What are you waiting for?")} </div>
         | RemoteData.Loading =>
           <div> {ReasonReact.string("Loading...")} </div>
         | RemoteData.Success(jokes) =>
           jokes
           |> List.map(j =>
                <Joke
                  joke=j
                  handleClick={joke => send(MarkFavorite(joke))}
                  isFavorite=false
                />
              )
           |> Array.of_list
           |> ReasonReact.array
         | RemoteData.Failure(_) =>
           <div> {ReasonReact.string("Ups ...")} </div>
         }}
      </div>
    </div>,
};