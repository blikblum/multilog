unit multilogAsm;
{§< Unit to "make a dent", in the call stack, with a function in assembler copied from JclDebug.pas.
This function is in its own unit, because there's the `$mode delphi` directive.}

{$mode delphi}

interface

{$REGION 'Xls: Comments section'}
{§ Explanations: function used only by the unit fileChannel.pas.}
{$ENDREGION}
function GetEBP: Pointer;

implementation

function GetEBP: Pointer;assembler;
{$asmMode Intel}
asm
        {$IFDEF CPU32}
        mov     eax,ebp
        {$ENDIF CPU32}
        {$IFDEF CPUx86_64}
        mov     rax,rbp
        {$ENDIF CPU64}
end;

end.

