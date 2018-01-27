program generator;

{$MODE OBJFPC} {$H+}

uses
  SysUtils,
  Classes,
  SyncObjs;

const
  PuncPredence = 3;
  OpPredence: array[0..5] of byte = (1, 1, 2, 2, 100, 100);
  Lookup: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', '+', '-', '*', '/', '(', ')');

type
  buf = array[1..10000] of ansistring;

  /// \brief A Small TStack for Small EOperand
  generic TStack<T> = class
    dat: array[1..10000] of T;
    n: longint;

  public
    constructor Create;
    destructor Destroy; override;

    procedure push(item: T);
    function pop: T;

    procedure empty;

    function top: T;
    property size: longint read n;
  end;

  ETTokenType = (num, opt, punct, eol, literal, invalid);
  EOp = (add, minus, mul, divide, equal, invalidEOp);
  EPunc = (openpunc, closepunc, invalidpunc = -1);

  TOpWrap = class
  public
    otype: EOp;
    predence: longword;

    constructor Create(optype: EOp; p: longword);
  end;

  TStackOp = specialize TStack<TOpWrap>;
  TStackNum = specialize TStack<extended>;

  TToken = class
    tt: ETTokenType;
    val: ansistring;

  public
    constructor Create(ptt: ETTokenType; pval: ansistring);

    property TType: ETTokenType read tt;

    function GetAsString: ansistring;
    function GetAsNumber: extended;
    function GetAsEPunc: EPunc;
    function GetAsEOp: EOp;
  end;

  TTokenizer = class
    type
    TTokenStack = specialize TStack<TToken>;

  var
    Val: ansistring;
    Pend: TTokenStack;
    Pos: int64;
    onedigit: boolean;

  protected
    function CreateNumToken(PVal: ansistring): TToken;
    function CreateOpToken(PVal: ansistring): TToken;
    function CreatePuncToken(PVal: ansistring): TToken;
    function CreateLitToken(PVal: ansistring): TToken;

    function LexNum: TToken;
    function LexOp: TToken;
    function LexPunc: TToken;
    function LexLit: TToken;

  public
    constructor Create(PVal: ansistring; od: boolean = False);
    destructor Destroy; override;

    procedure Reconstruct(PVal: ansistring);

    function Next: TToken;
    function peek: TToken;

    procedure revert(last: TToken);

    property Position: int64 read Pos;

  end;

  TCalculator = class;

  TChromo = class
    bits, decoded: ansistring;
    fit: real;
    calc: TCalculator;
    k: extended;
    onedigit: boolean;

  public
    constructor Create(pbits: ansistring; pfit: real; target: extended;
      od: boolean = False);
    destructor Destroy; override;

    property fitness: real read fit;
    property bit: ansistring read bits;
    property DecodedChromo: ansistring read decoded;

    function parse(var n: longint): buf;
    procedure work;
  end;

  APopulation = array[1..10000] of TChromo;
  PPopulation = ^APopulation;

  TCalculator = class
    EOps: TStackOp;
    tl: longword;
    nums: TStacknum;
    t: TTokenizer;
    onedigit: boolean;

  protected
    procedure parseEOp(tt: EOp; puncl: longword);
  public
    constructor Create(od: boolean = False);
    destructor Destroy; override;

    function calculate(s: ansistring; b: pansistring): extended;
  end;

  PReal = ^Real;

  TGenetic = class
    type
    /// Each TWorker does and shares work process with another
    TWorker = class(TThread)
    protected
      st, fin, crr: longint;
      ppop: ppopulation;
      ptfit: preal;

      term: boolean;

      found: boolean;
      gennew: boolean;

      procedure printresult;
      procedure Execute; override;
      procedure addfit;

    public
      procedure ThreadTerminatedNotifyEvent(Sender: TObject);
      property Terminated: boolean read term;
      constructor Create(var pop: APopulation; var fit: real;
        lstart, lfin: longint; gennewl: boolean = False);

      property foundres: boolean read found;
    end;

  private
    type
    AWorkers = array[1..100] of TWorker;

  var
    pop: APopulation;
    workers: AWorkers;
    gen, popsize: longint;
    k: extended;
    found: boolean;
    tfit: real;

  protected
    procedure mutate(var s: ansistring);
    procedure crossover(var s, s1: ansistring);
    function roulette(tf: real): ansistring;
    function GenerateRandomTChromo(len: longint): TChromo;

  public
    constructor Create(Target: extended; od: boolean = False);
    procedure Work;

    property Generation: longint read gen;
  end;

var
  crossrate: real = 0.3;
  mutrate: real = 0.001;
  gensize: integer = 30;
  chromolen: longint = 24;
  genelen: integer = 4;
  maxgen: integer = 10;
  workerslen: integer = 5;
  onedigit: boolean = True;
  cs: TCriticalSection;
  gen: TGenetic;

  constructor TGenetic.TWorker.Create(var pop: APopulation; var fit: real;
    lstart, lfin: longint; gennewl: boolean);
  begin
    inherited Create(False);

    term := False;
    st := lstart;
    fin := lfin;
    ppop := @pop;
    ptfit := @fit;
    gennew := gennewl;
    FreeOnTerminate := False;
    found := False;
    OnTerminate := @Self.ThreadTerminatedNotifyEvent;

    cs := TCriticalSection.Create;
  end;

  procedure TGenetic.TWorker.printresult;
  begin
    writeln(ppop^[crr].decoded);
  end;

  procedure TGenetic.TWorker.addfit;
  begin
    ptfit^ := ptfit^ + ppop^[crr].fitness;
  end;

  procedure TGenetic.TWorker.Execute;
  var
    i: longint;
  begin
    for i := st to fin do
    begin
      crr := i;
      ppop^[i].work;

      if (ppop^[i].fitness = 999.0) then
      begin
        found := True;

        cs.Acquire;
        PrintResult;
        cs.Release;
      end;

      cs.Acquire;
      addfit;
      cs.Release;

    end;

    term := True;
  end;

  procedure TGenetic.TWorker.ThreadTerminatedNotifyEvent(Sender: TObject);
  begin
    //writeln('Thread ', self.ThreadID, ' terminated');
  end;

  destructor TTokenizer.Destroy;
  begin
    pend.Destroy;
  end;

  destructor TCalculator.Destroy;
  begin
    t.Destroy;
    EOps.Destroy;
    nums.Destroy;
  end;

  constructor TStack.Create;
  begin
    n := 0;
  end;

  destructor TStack.Destroy;
  begin
    n := 0;
  end;

  procedure TStack.empty;
  begin
    n := 0;
  end;

  function TStack.top: T;
  begin
    if (n > 0) then
      exit(dat[n]);
    exit(default(T));
  end;

  procedure TStack.push(item: T);
  begin
    n := n + 1;
    dat[n] := item;
  end;

  function TStack.pop: T;
  begin
    if (n > 0) then
    begin
      n := n - 1;
      exit(dat[n + 1]);
    end;

    exit(default(T));
  end;

  constructor TChromo.Create(pbits: ansistring; pfit: real; target: extended;
    od: boolean = False);
  begin
    self.bits := pbits;
    self.fit := pfit;
    calc := TCalculator.Create;
    self.k := target;
    self.onedigit := od;
  end;

  constructor TToken.Create(ptt: ETTokenType; pval: ansistring);
  begin
    self.tt := ptt;
    self.val := pval;
  end;

  function TToken.GetAsString: ansistring;
  begin
    exit(Val);
  end;

  function TToken.GetAsNumber: extended;
  begin
    Exit(StrToFloat(Val));
  end;

  function TToken.GetAsEOp: EOp;
  begin
    case Val of
      '+': exit(add);
      '-': exit(minus);
      '*': exit(mul);
      '/': exit(divide);
      '=': exit(equal);
      else
        exit(invalidEOp);
    end;
  end;

  function TToken.GetAsEPunc: EPunc;
  begin
    case Val of
      '(': exit(openpunc);
      ')': exit(closepunc);
      else
        exit(invalidpunc);
    end;
  end;

  constructor TTokenizer.Create(PVal: ansistring; od: boolean = False);
  begin
    Self.Pos := 0;
    Self.Val := PVal;
    Self.Pend := TTokenStack.Create;
    Self.OneDigit := od;
  end;

  function TTokenizer.CreateLitToken(PVal: ansistring): TToken;
  begin
    exit(TToken.Create(literal, PVal));
  end;

  function TTokenizer.CreateNumToken(PVal: ansistring): TToken;
  begin
    exit(TToken.Create(num, PVal));
  end;

  function TTokenizer.CreateOpToken(PVal: ansistring): TToken;
  begin
    exit(TToken.Create(opt, PVal));
  end;

  function TTokenizer.CreatePuncToken(PVal: ansistring): TToken;
  begin
    exit(TToken.Create(punct, PVal));
  end;

  function TTokenizer.LexNum: TToken;
  var
    t: ansistring;
  begin
    while
      ((Val[Pos] in ['0'..'9']) or ((Val[Pos] = '.') and
        (System.Pos(t, '.') = 0))) do
    begin
      t := t + Val[Pos];
      Pos := Pos + 1;

      if (Pos > Length(Val)) then
        break;

      if (onedigit) then
        break;
    end;

    Pos := Pos - 1;

    exit(CreateNumToken(t));
  end;

  function TTokenizer.LexLit: TToken;
  var
    t: ansistring;
  begin
    while
      (Val[Pos] in ['A'..'Z', 'a'..'z']) do
    begin
      t := t + Val[Pos];
      Pos := Pos + 1;

      if (Pos > Length(Val)) then
        break;
    end;

    Pos := Pos - 1;

    exit(CreateLitToken(t));
  end;

  function TTokenizer.LexOp: TToken;
  begin
    exit(CreateOpToken(Val[Pos]));
  end;

  function TTokenizer.LexPunc: TToken;
  begin
    exit(CreatePuncToken(Val[Pos]));
  end;

  function TTokenizer.Next: TToken;
  begin
    if (pend.size > 0) then
      exit(pend.pop);

    Pos := Pos + 1;

    if (Pos > Length(Val)) then
      exit(TToken.Create(eol, '-1'));
    while (Val[Pos] = ' ') and (Pos < Length(Val)) do
      Pos := Pos + 1;
    if (Pos > Length(Val)) then
      exit(TToken.Create(eol, '-1'));

    if (Val[Pos] >= '0') and (Val[Pos] <= '9') then
      exit(LexNum)
    else if Val[Pos] in ['+', '-', '*', '/', '='] then
      exit(LexOp)
    else if (Val[Pos] in ['a'..'z']) or (Val[Pos] in ['A'..'Z']) then
      exit(LexLit)
    else
      exit(LexPunc);

    if (Pos > Length(Val)) then
      exit(TToken.Create(eol, '-1'));

    exit(TToken.Create(invalid, Val[Pos]));
  end;

  function TTokenizer.peek: TToken;
  var
    t: TToken;
  begin
    t := Next;
    pend.push(t);

    exit(t);
  end;

  procedure TTokenizer.revert(last: TToken);
  begin
    pend.push(last);
  end;

  procedure TTokenizer.Reconstruct(PVal: ansistring);
  begin
    pend.empty;
    Val := PVal;
    Pos := 0;
  end;

  constructor TOpWrap.Create(optype: EOp; p: longword);
  begin
    self.otype := optype;
    self.predence := p;
  end;

  procedure TCalculator.parseEOp(tt: EOp; puncl: longword);
  var
    t1, t2: real;
  begin
    while (EOps.size > 0) and ((t.Position > tl) or
        (EOps.top.predence >= OpPredence[Ord(tt)] + puncl * PuncPredence)) do
    begin
      t1 := nums.pop;
      t2 := nums.pop;

      if (EOps.top.otype = add) then
      begin
        EOps.pop;
        nums.push(t1 + t2);
      end
      else if (EOps.top.otype = minus) then
      begin
        EOps.pop;
        nums.push(t2 - t1);
      end
      else if (EOps.top.otype = mul) then
      begin
        EOps.pop;
        nums.push(t1 * t2);
      end
      else if (EOps.top.otype = divide) then
      begin
        EOps.pop;
        if (t1 = 0) then
          nums.push(t2)
        else
          nums.push(t2 / t1);
      end;
    end;
  end;

  constructor TCalculator.Create(od: boolean = False);
  begin
    nums := TStackNum.Create;
    EOps := TStackOp.Create;
    onedigit := od;
    t := TTokenizer.Create('', onedigit);

    inherited Create;
  end;

  function TCalculator.calculate(s: ansistring; b: pansistring): extended;
  var
    ttt, tt: TToken;
    punclevel: word;
  begin
    EOps.empty;
    nums.empty;

    punclevel := 0;
    t.Reconstruct(s);

    repeat
      tt := t.Next;

      if (tt.ttype = eol) then
      begin
        parseEOp(tt.GetAsEOp, punclevel);
        break;
      end;
      if (tt.ttype = num) then
      begin
        if (b <> nil) then
          b^ := b^ + FloatToStr(tt.GetAsNumber);
        nums.push(tt.GetAsNumber);
      end
      else
      begin
        if (tt.ttype = punct) and (tt.GetAsEPunc = openpunc) then
        begin
          punclevel := punclevel + 1;
        end
        else if (tt.ttype = punct) and (tt.GetAsEPunc = closepunc) then
        begin
          punclevel := punclevel - 1;
        end
        else if (tt.ttype = opt) then
        begin
          ttt := t.peek;

          if (ttt.ttype = opt) then
          begin
            if ((tt.GetAsEOp = minus) and (ttt.GetAsEOp = minus)) or
              ((tt.GetAsEOp = add) and (ttt.GetAsEOp = add)) then
            begin
              t.revert(TToken.Create(num, '+'));
              parseEOp(add, punclevel);
              EOps.push(TOpWrap.Create(add, 1 + punclevel * 3));
            end
            else if ((tt.GetAsEOp = minus) and (ttt.GetAsEOp = add)) or
              ((tt.GetAsEOp = add) and (ttt.GetAsEOp = minus)) then
            begin
              t.revert(TToken.Create(num, '-'));
              parseEOp(minus, punclevel);
              EOps.push(TOpWrap.Create(minus, 1 + punclevel * 3));
            end;
          end
          else
          begin
            parseEOp(tt.GetAsEOp, punclevel);
            EOps.push(TOpWrap.Create(tt.GetAsEOp, OpPredence[integer(tt.GetAsEOp)] +
              punclevel * 3));
          end;
        end;

        if (b <> nil) then
          b^ := b^ + tt.GetAsString;
      end;

    until tt.ttype = eol;

    exit(nums.top);
  end;

  function getrand: real;
  begin
    exit(random);
  end;

  function BinToDec(s: ansistring): byte;
  begin
    case (s) of
      '0000': exit(0);
      '0001': exit(1);
      '0010': exit(2);
      '0011': exit(3);
      '0100': exit(4);
      '0101': exit(5);
      '0110': exit(6);
      '0111': exit(7);
      '1000': exit(8);
      '1001': exit(9);
      '1010': exit(10);
      '1011': exit(11);
      '1100': exit(12);
      '1101': exit(13);
      '1110': exit(14);
      '1111': exit(15);
    end;

    exit(0);
  end;

  destructor TChromo.Destroy;
  begin
    calc.Destroy;
  end;

  function TChromo.parse(var n: longint): buf;
  var
    i, state, br: longint;
    t: int64;
    s: ansistring;
    tkn: TTokenizer;
    tk: array[1..10000] of TToken;
    tt: TToken;
    nt: longint;
  begin
    i := 1;

    state := 0;
    br := 0;
    n := 0;
    nt := 0;

    while (i <= chromolen + 1) do
    begin
      t := BinToDec(Copy(bits, i, genelen));
      i := i + genelen;

      s := s + lookup[t];
    end;

    tkn := TTokenizer.Create(s);

    repeat
      tt := tkn.peek;

      if tt.ttype <> eol then
      begin
        tt := tkn.Next;
        nt := nt + 1;
        tk[nt] := tt;
      end;
    until tt.ttype = eol;

    for i := 1 to nt do
    begin
      if (state = 1) then
      begin
        if (tk[i].ttype = num) or (tk[i].ttype = punct) then
          continue
        else
        begin
          state := 0;
          Inc(n);
          parse[n] := tk[i].GetAsString;
          continue;
        end;
      end
      else if (state = 0) then
      begin
        if (tk[i].ttype = opt) or ((tk[i].ttype = punct) and
          (tk[i].GetAsEPunc = closepunc)) then
          continue
        else
        begin
          if (tk[i].ttype = punct) then
          begin
            br := br + 1;
            state := 3;
            Inc(n);
            parse[n] := tk[i].GetAsString;
            continue;
          end;

          state := 1;
          Inc(n);
          parse[n] := tk[i].GetAsString;
          continue;
        end;
      end
      else if (state = 3) then
      begin
        if (tk[i].ttype = num) or ((tk[i].ttype = punct) and
          (tk[i].GetAsEPunc = closepunc)) then
        begin
          if (tk[i].ttype = punct) and (br > 0) then
            br := br - 1;

          state := 1;
          Inc(n);
          parse[n] := tk[i].GetAsString;
          continue;
        end
        else
          continue;
      end;
    end;

    for i := 1 to n do
    begin
      if (parse[i] = '/') and (parse[i + 1] = '0') then
        parse[i] := '+'
      else if (parse[i] = '(') and (parse[i + 1] = ')') then
      begin
        parse[i] := '0';
        parse[i + 1] := '0';
      end;
    end;

    while (br > 0) do
    begin
      br := br - 1;
      n := n + 1;
      parse[n] := ')';
    end;

    tkn.Destroy;
    tt.Destroy;
  end;

  procedure TChromo.work;
  var
    b: buf;
    i, n: longint;
    s, st: ansistring;
    r: extended;
  begin
    n := 0;
    b := parse(n);

    for i := 1 to n do
      s := s + b[i];

    r := calc.calculate(s, @st);

    decoded := st;

    if (r = k) then
      fit := 999.0
    else
      fit := 1 / abs(r - k);
  end;

  function TGenetic.GenerateRandomTChromo(len: longint): TChromo;
  var
    b: ansistring;
    c: TChromo;
    i: longint;
  begin
    for i := 1 to len + len mod 2 do
      if getrand > 0.5 then
        b := b + '1'
      else
        b := b + '0';

    c := TChromo.Create(b, 0, k, onedigit);

    exit(c);
  end;

  procedure TGenetic.mutate(var s: ansistring);
  var
    i: longint;
  begin
    for i := 1 to length(s) do
    begin
      if getrand < mutrate then
      begin
        if (s[i] = '1') then
          s[i] := '0'
        else
          s[i] := '1';
      end;
    end;
  end;

  procedure TGenetic.crossover(var s, s1: ansistring);
  var
    pivot: longint;
    t1, t2: ansistring;
  begin
    if (getrand < crossrate) then
    begin
      pivot := round(getrand * chromolen);

      t1 := Copy(s, 1, pivot) + Copy(s1, pivot + 1, chromolen);
      t2 := Copy(s1, 1, pivot) + Copy(s, pivot + 1, chromolen);

      s := t1;
      s1 := t2;
    end;
  end;

  function TGenetic.roulette(tf: real): ansistring;
  var
    slice: real;
    fsf: real;
    i: longint;
  begin
    slice := getrand * tf;
    fsf := 0;

    for i := 1 to popsize do
    begin
      fsf := fsf + pop[i].fitness;

      if (fsf > slice) then
        exit(pop[i].bit);
    end;

    exit(pop[i].bit);
  end;

  constructor TGenetic.Create(target: extended; od: boolean = False);
  var
    i: longint;
  begin
    k := target;
    gen := 0;
    onedigit := od;

    popsize := gensize + gensize mod 2;

    for i := 1 to popsize do
    begin
      pop[i] := GenerateRandomTChromo(chromolen);
    end;
  end;

  procedure TGenetic.work;
  var
    nsize: longint;
    temp: APopulation;
    o1, o2: ansistring;
    i: longint;
  begin
    nSize := 0;
    tfit := 0;
    found := False;

    while not found do
    begin
      for i := 1 to workerslen - 1 do
      begin
        workers[i] := TWorker.Create(pop, tfit, popsize div
          workerslen * (i - 1) + 1, popsize div workerslen * i);

      end;

      workers[workerslen] := TWorker.Create(pop, tfit, popsize div
        workerslen * (workerslen - 1) + 1, popsize);

      for i := 1 to workerslen do
      begin
        workers[i].waitfor;

        if (workers[i].foundres) then
        begin
          found := True;
          break;
        end;

        workers[i].Destroy;
      end;

      nSize := 0;

      repeat
        o1 := roulette(tfit);
        o2 := roulette(tfit);

        crossover(o1, o2);

        mutate(o1);
        mutate(o2);

        temp[nsize + 1] := TChromo.Create(o1, 0, k);
        temp[nsize + 2] := TChromo.Create(o2, 0, k);

        nsize := nsize + 2;
      until nsize >= gensize;

      for i := 1 to popsize do
        pop[i].Destroy;

      pop := temp;
      popsize := gensize + gensize mod 2;

      Inc(gen);

      if (gen = maxgen) then
      begin
        found := False;
        break;
      end;
    end;

    if (found = False) then
      writeln('NaN'){ else writeln(gen) };
  end;

var
  k: extended;

  procedure ParseParams;
  var
    s, stemp: ansistring;
    t: TTokenizer;
    tt: TToken;
    i: longint;
  begin
    for i := 1 to ParamCount do
    begin
      s := s + ' ' + ParamStr(i);
    end;

    if (s = '') then
      exit;

    i := 0;
    t := TTokenizer.Create(s);

    repeat
      tt := t.peek;
      Inc(i);

      if (tt.GetAsEOp = minus) then
      begin
        tt := t.Next;
        tt := t.peek;

        if (tt.ttype = literal) then
        begin
          stemp := tt.GetAsString;

          tt := t.Next;
          tt := t.peek;

          Inc(i);

          if (tt.ttype = opt) and (tt.GetAsEOp = equal) then
          begin
            tt := t.Next;
            tt := t.peek;

            Inc(i);

            if (tt.ttype = num) then
            begin
              case stemp of
                'genelen': genelen :=
                    Round(tt.GetAsNumber);
                'gensize': gensize :=
                    Round(tt.GetAsNumber);
                'mutrate': mutrate :=
                    tt.GetAsNumber;
                'crossrate': crossrate :=
                    tt.GetAsNumber;
                'chromolen': chromolen :=
                    Round(tt.GetAsNumber);
                'maxgen': maxgen :=
                    Round(tt.GetAsNumber);
                'onedigit':
                begin
                  if (tt.GetAsNumber >= 1) then
                    onedigit := True
                  else
                    onedigit := False;
                end;
                'workers': workerslen :=
                    Round(tt.GetAsNumber);
              end;
            end;
          end
          else;
        end;
      end
      else if (tt.ttype = num) then
      begin
        tt := t.Next;
        k := tt.GetAsNumber;
      end;
    until tt.ttype = eol;

    t.Destroy;
    tt.Destroy;
  end;

  procedure init;
  begin
    randomize;
    cs := TCriticalSection.Create;
    parseparams;
  end;

  procedure generate;
  begin
    gen := TGenetic.Create(k, onedigit);
    gen.Work;
  end;

  procedure cleanup;
  begin
    cs.Destroy;
    gen.Destroy;
  end;

begin
  init;
  generate;
  cleanup;
end.
