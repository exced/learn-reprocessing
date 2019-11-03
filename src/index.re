open Reprocessing;
open Printf;

type state = {
  time: float,
  lines: array(string),
  cursor: (int, int),
};

let setup = (env) => {
  Env.size(~width=500, ~height=300, env);
  {lines: [|""|], time: 0., cursor: (0, 0)}
};


module Option = {
  type t('a) = option('a);
  let iter = (t, ~f) => switch (t) {
    | None => ()
    | Some(a) => f(a)
  }
};

let constrainCursor = (lines, (row, col)) => {
  let row = max(0, min(Array.length(lines) - 1, row));
  let rowContents = Array.get(lines, row);
  let col = max(0, min(String.length(rowContents), col));
  (row, col)
};

let updateLines = (~lines, ~cursor, f) => {
  let linesLength = Array.length(lines);
  let (row, col) = constrainCursor(lines, cursor);
  let (before, after) = {
    let rowContents = try (Array.get(lines, row)) {
      | Invalid_argument(_) => failwith("Invalid row somehow " ++ string_of_int(row));
    };
    let rLen = String.length(rowContents);
    let before = String.sub(rowContents, 0, col);
    let after = String.sub(rowContents, col, rLen - col);
    (before, after)
  };
  switch (f(before, after)) {
    | `Delete(addToPreviousLine) =>
      // remove row
      let newLines = if (linesLength > 1) {
        let newLines = Array.make(linesLength - 1, "");
        Array.blit(lines, 0, newLines, 0, row);
        Array.blit(lines, row + 1, newLines, row, linesLength - row - 1);
        newLines
      } else {
        [|""|]
      };
      let prevRow = max(0, row - 1);
      let prevRowContents = Array.get(newLines, prevRow);
      Array.set(newLines, prevRow, prevRowContents ++ addToPreviousLine);
      (newLines, (prevRow, String.length(prevRowContents)))
    
    | `Edit(before, after) =>
      // update row
      let newString = before ++ after;
      Array.set(lines, row, newString);
      (lines, (row, String.length(before)))
    
    | `Split(changedRow, newRow, indent) =>
      // Edit and add row
      let indentStr = String.make(indent, ' ');
      let newLines = Array.make(linesLength + 1, "");
      Array.blit(lines, 0, newLines, 0, row);
      Array.set(newLines, row, changedRow);
      Array.set(newLines, row + 1, indentStr ++ newRow);
      Array.blit(lines, row + 1, newLines, row + 2, linesLength - row - 1);
      (newLines, (row + 1, indent))
  }
};

let rec countLeadingSpaces = (s, i) => {
  if (i < String.length(s) - 1 && String.get(s, i) == ' ') {
    countLeadingSpaces(s, i + 1)
  } else {
    i
  }
};

// TODO: select mode
// addChar -> delete selection, add char
// delete/backspace -> delete selection
// enter -> delete selection, split
// arrow -> de-select (different end location per arrow!)
// arrow + shift -> select more or less...
// 
// Either:
// - cursor manipulation
// - delete and then do stuff
// OR TAB/Shift-tab

module Location = {
  type t = {
    row: int,
    col: int
  };

  let constrain = (lines, {row, col}) => {
    let row = max(0, min(Array.length(lines) - 1, row));
    let rowContents = Array.get(lines, row);
    let col = max(0, min(String.length(rowContents), col));
    {row, col}
  };

  let (<) = (a, b) =>
    a.col < b.col || (a.col == b.col && a.row < b.row);
    
}

type cursor =
  [ `Normal(location)
  | `Select(location, location)];

let deleteSelection = (lines, `Select(firstLoc, lastLoc)) => {
  let firstLoc = constrainLocation(lines, firstLoc);
  let lastLoc = constrainLocation(lines, lastLoc);
  let lengthLines
  let newLines = Array.make(linesLength - 1, "");
  Array.blit(lines, 0, newLines, 0, row);
  Array.blit(lines, row + 1, newLines, row, linesLength - row - 1);
  newLines
  // TODO delete rows between
  // TODO delete after leftloc
  // TODO delete before rightloc
};

// let handleSelect = (key, lines, selectDir, leftLoc, rightLoc) => {
//   switch (key) {
//     | `Letter(c) => ()
//     | _ => ()
//   }
// };

let keyTyped = ({lines, cursor} as state, env) => {
  let updateLines = updateLines(~lines, ~cursor);
  let addChar = (c) => updateLines((before, after) => `Edit(before ++ String.make(1, c), after));
  let (lines, cursor) = switch (Key.kind(Env.keyCode(env))) {
    | `Letter(c) when Key.modifier(`Shift, env) => addChar(Char.uppercase(c))
    | `Letter(c) => addChar(c)
    | `Num(n) when Key.modifier(`Shift, env) => 
      let shiftNumbers = [|')', '!', '@', '#', '$', '%', '^', '&', '*', '('|];
      addChar(Array.get(shiftNumbers, n))
    | `Num(n) => addChar(String.get(string_of_int(n), 0))
    | `Tab => updateLines((before, after) => `Edit(before ++ "  ", after));
    | `Space => addChar(' ')
    | `Enter =>
      // Regular: updateLines((before, after) => `Split(before, after));
      updateLines((before, after) => {
        let leading = countLeadingSpaces(before, 0);
        `Split(before, after, leading)
        });
      
    | `Backspace =>
      updateLines(
        (before, after) => before == "" ? 
          `Delete(after) : 
          `Edit(String.sub(before, 0, String.length(before) - 1), after));
    | `Delete => 
      updateLines(
        (before, after) =>
          `Edit(before, after == "" ? "" :
            String.sub(after, 1, String.length(after) - 1)))
    | `Arrow(dir) => 
      let (row, col) = cursor;
      let (rowOffset, colOffset) = switch (dir) {
        | `Left => (0, -1)
        | `Right => (0, 1)
        | `Up => (-1, 0)
        | `Down => (1, 0)
      };
      (lines, constrainCursor(lines, (row + rowOffset, col + colOffset)))
    | `Quote as e
    | `Comma as e
    | `Minus as e
    | `Period as e
    | `Slash as e
    | `Semicolon as e
    | `Equals as e
    | `OpenBracket as e
    | `CloseBracket as e
    | `Backslash as e
    | `Backtick as e =>
      let (normal, shifted) = Key.symbols(e);
      addChar(Key.modifier(`Shift, env) ? shifted : normal) 
    | _ => 
      print_endline("Unknown key");
      (lines, cursor)
  };

  {...state, lines, cursor}
};

let draw = (state, env) => {
  Draw.background(Constants.white, env);
  
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=100), env);
  let row = fst(state.cursor);
  let beforeText = String.sub(Array.get(state.lines, row), 0, snd(state.cursor));
  let w = Draw.textWidth(~body=beforeText, env);
  Draw.rect(~pos=(w + 11, 30 * row), ~width=15, ~height=30, env);

  Array.iteri((i, str) => Draw.text(~body=str, ~pos=(10, 30 * i), env),
    state.lines);

  state
};

run(~setup, ~draw, ~keyTyped, ());
