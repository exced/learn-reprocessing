let kind = fun
    | Reprocessing_Events.Backspace => `Backspace
    | Tab => `Tab
    | Enter => `Enter
    | Escape => `Escape
    | Space => `Space
    | Quote => `Quote
    | Comma => `Comma
    | Minus => `Minus
    | Period => `Period
    | Slash => `Slash
    | Semicolon => `Semicolon
    | Equals => `Equals
    | OpenBracket => `OpenBracket
    | Backslash => `Backslash
    | CloseBracket => `CloseBracket
    | Backtick => `Backtick
    | Num_0 => `Num(0)
    | Num_1 => `Num(1)
    | Num_2 => `Num(2)
    | Num_3 => `Num(3)
    | Num_4 => `Num(4)
    | Num_5 => `Num(5)
    | Num_6 => `Num(6)
    | Num_7 => `Num(7)
    | Num_8 => `Num(8)
    | Num_9 => `Num(9)
    | A => `Letter('a')
    | B => `Letter('b')
    | C => `Letter('c')
    | D => `Letter('d')
    | E => `Letter('e')
    | F => `Letter('f')
    | G => `Letter('g')
    | H => `Letter('h')
    | I => `Letter('i')
    | J => `Letter('j')
    | K => `Letter('k')
    | L => `Letter('l')
    | M => `Letter('m')
    | N => `Letter('n')
    | O => `Letter('o')
    | P => `Letter('p')
    | Q => `Letter('q')
    | R => `Letter('r')
    | S => `Letter('s')
    | T => `Letter('t')
    | U => `Letter('u')
    | V => `Letter('v')
    | W => `Letter('w')
    | X => `Letter('x')
    | Y => `Letter('y')
    | Z => `Letter('z')
    | Right => `Arrow(`Right)
    | Left => `Arrow(`Left)
    | Down => `Arrow(`Down)
    | Up => `Arrow(`Up)
    | RightCtrl
    | LeftCtrl => `Ctrl
    | RightShift
    | LeftShift => `Shift
    | RightAlt
    | LeftAlt => `Alt
    | RightOsKey
    | LeftOsKey => `OsKey
    | CapsLock => `CapsLock
    | Nothing => `Delete; // WHAT

let modifier = (key, env) => {
    open Reprocessing;
    switch (key) {
    | `Shift => Env.key(LeftShift, env) || Env.key(RightShift, env)
    | `Ctrl => Env.key(LeftCtrl, env) || Env.key(RightCtrl, env)
    | `Alt => Env.key(LeftAlt, env) || Env.key(RightAlt, env)
    };
}

let symbols = fun
  | `Quote => ('\'', '"')
  | `Comma => (',', '<')
  | `Minus => ('-', '_')
  | `Period => ('.', '>')
  | `Slash => ('/', '?')
  | `Semicolon => (';', ':')
  | `Equals => ('=', '+')
  | `OpenBracket => ('[', '{')
  | `CloseBracket => (']', '}')
  | `Backslash => ('\\', '|')
  | `Backtick => ('`', '~');