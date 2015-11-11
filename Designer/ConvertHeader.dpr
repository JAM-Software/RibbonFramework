program ConvertHeader;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  RibbonCompiler in 'Classes\RibbonCompiler.pas';

var
  compiler: TRibbonCompiler;

begin
  try
    if ParamCount <> 2 then begin
      Writeln('Usage: ConvertHeader <header.h> <header.pas>');
      Halt(1);
    end
    else begin
      compiler := TRibbonCompiler.Create;
      try
        if not compiler.ConvertHeaderFile(ParamStr(1), ParamStr(2)) then
          Halt(1);
      finally
        compiler.Free;
      end;
    end;
  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
