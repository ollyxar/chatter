{*
 * Chatter - long-polling chat server
 *
 * (c) Olexy Sviridenko aka Alex Slipknot
 * alexslipknot@europe.com
 *}

Program chatter;
{$define UseCThreads}
uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, chattermapperunit, chatterunit;

{$R *.res}

begin
  Application.Title := 'Chatter';
  Application.Initialize;
  Application.Run;
end.
