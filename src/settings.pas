unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSettings = packed record
    MessagesLimit: LongWord;
    PostMaxLength: Word;
    TimeLimit: Word;
    Interval: Word;
    Port: Word;
    Secret: string;
    WelcomeName: string;
    WelcomeAvatar: string;
    WelcomePost: string;
  end;

implementation

end.

