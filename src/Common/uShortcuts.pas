{ ═══════════════════════════════════════════════════════════════════════════════
  uShortcuts.pas - Keyboard Shortcuts Manager

  Part of 3nity Media - Lazarus Edition

  This unit manages customizable keyboard shortcuts for the application.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uShortcuts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LCLType, Menus;

type
  { Shortcut action identifiers }
  TShortcutAction = (
    { Playback }
    saPlayPause,
    saStop,
    saPrevious,
    saNext,
    saSeekForward,
    saSeekBackward,
    saSeekForward5,
    saSeekBackward5,
    saSpeedUp,
    saSpeedDown,
    saSpeedReset,
    saSetLoopA,
    saSetLoopB,
    saClearLoop,
    saFrameForward,
    saFrameBackward,
    saPrevChapter,
    saNextChapter,
    saGotoTime,
    { Audio }
    saVolumeUp,
    saVolumeDown,
    saMute,
    saAudioDelayPlus,
    saAudioDelayMinus,
    saAudioDelayReset,
    { Video }
    saFullscreen,
    saRotate,
    saDeinterlace,
    saZoomIn,
    saZoomOut,
    saZoomReset,
    saAlwaysOnTop,
    saFitToVideo,
    saScreenshot,
    saVideoAdjust,
    { Visualization }
    saVisNextMode,
    saVisNextColor,
    { Subtitles }
    saSubtitleDelayPlus,
    saSubtitleDelayMinus,
    saSubtitleDelayReset,
    { DVD Navigation }
    saDVDMenu,
    saDVDUp,
    saDVDDown,
    saDVDLeft,
    saDVDRight,
    saDVDSelect,
    { Window }
    saOpenFile,
    saOpenURL,
    saPlaylist,
    saQuit
  );

  TShortcutItem = record
    Action: TShortcutAction;
    Key: Word;
    Shift: TShiftState;
    DefaultKey: Word;
    DefaultShift: TShiftState;
  end;

  { TShortcutManager }
  TShortcutManager = class
  private
    FShortcuts: array[TShortcutAction] of TShortcutItem;
    FFileName: string;
    procedure InitializeDefaults;
    function GetShortcut(Action: TShortcutAction): TShortcutItem;
    procedure SetShortcut(Action: TShortcutAction; Key: Word; Shift: TShiftState);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure Load;
    procedure Save;
    procedure ResetToDefaults;
    procedure ResetAction(Action: TShortcutAction);

    function FindAction(Key: Word; Shift: TShiftState): TShortcutAction;
    function ActionExists(Key: Word; Shift: TShiftState; ExcludeAction: TShortcutAction): Boolean;
    function MatchesShortcut(Key: Word; Shift: TShiftState; Action: TShortcutAction): Boolean;
    function ShortcutToText(Action: TShortcutAction): string;
    function GetActionName(Action: TShortcutAction): string;
    function GetActionCategory(Action: TShortcutAction): string;

    property Shortcuts[Action: TShortcutAction]: TShortcutItem read GetShortcut;
    property FileName: string read FFileName;

    procedure AssignShortcut(Action: TShortcutAction; Key: Word; Shift: TShiftState);
    procedure ClearShortcut(Action: TShortcutAction);
  end;

var
  ShortcutManager: TShortcutManager;

function KeyToText(Key: Word; Shift: TShiftState): string;
function TextToKey(const Text: string; out Key: Word; out Shift: TShiftState): Boolean;

implementation

uses
  uLocale, uConfig, uMPVConst;

const
  SECTION_SHORTCUTS = 'Shortcuts';

function KeyToText(Key: Word; Shift: TShiftState): string;
var
  S: string;
begin
  S := '';

  if ssCtrl in Shift then S := S + 'Ctrl+';
  if ssAlt in Shift then S := S + 'Alt+';
  if ssShift in Shift then S := S + 'Shift+';

  case Key of
    VK_SPACE: S := S + 'Space';
    VK_RETURN: S := S + 'Enter';
    VK_ESCAPE: S := S + 'Escape';
    VK_TAB: S := S + 'Tab';
    VK_BACK: S := S + 'Backspace';
    VK_DELETE: S := S + 'Delete';
    VK_INSERT: S := S + 'Insert';
    VK_HOME: S := S + 'Home';
    VK_END: S := S + 'End';
    VK_PRIOR: S := S + 'Page Up';
    VK_NEXT: S := S + 'Page Down';
    VK_LEFT: S := S + 'Left';
    VK_RIGHT: S := S + 'Right';
    VK_UP: S := S + 'Up';
    VK_DOWN: S := S + 'Down';
    VK_F1..VK_F12: S := S + 'F' + IntToStr(Key - VK_F1 + 1);
    VK_NUMPAD0..VK_NUMPAD9: S := S + 'Num ' + IntToStr(Key - VK_NUMPAD0);
    VK_ADD: S := S + 'Num +';
    VK_SUBTRACT: S := S + 'Num -';
    VK_MULTIPLY: S := S + 'Num *';
    VK_DIVIDE: S := S + 'Num /';
    VK_OEM_PLUS: S := S + '+';
    VK_OEM_MINUS: S := S + '-';
    VK_OEM_PERIOD: S := S + '.';
    VK_OEM_COMMA: S := S + ',';
    VK_OEM_4: S := S + '[';
    VK_OEM_6: S := S + ']';
    VK_OEM_1: S := S + ';';
    VK_OEM_2: S := S + '/';
    VK_OEM_3: S := S + '`';
    VK_OEM_5: S := S + '\';
    VK_OEM_7: S := S + '''';
    Ord('A')..Ord('Z'): S := S + Chr(Key);
    Ord('0')..Ord('9'): S := S + Chr(Key);
    0: S := '(None)';
  else
    S := S + Format('Key %d', [Key]);
  end;

  Result := S;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  TextToKey - Parse a text representation of a keyboard shortcut

  Purpose: Converts a human-readable shortcut string (e.g., "Ctrl+Alt+A") back
           into its component parts: virtual key code and modifier state.
           This is the inverse operation of KeyToText.

  Parameters:
    - Text: The shortcut string to parse (e.g., "Ctrl+Shift+F1", "Space", "Alt+M")
    - Key: Output parameter receiving the virtual key code (VK_* constant)
    - Shift: Output parameter receiving modifier state (ssCtrl, ssAlt, ssShift)

  Returns: True if parsing succeeded, False if the key part is unrecognized.
           Returns True with Key=0 for empty strings or "(None)".

  Parsing Process:
    1. Normalize input to uppercase
    2. Strip modifier prefixes one by one (CTRL+, ALT+, SHIFT+)
    3. Map remaining key name to VK_* constant

  Supported Key Names:
    - Letters: A-Z (case insensitive)
    - Numbers: 0-9
    - Function keys: F1-F12
    - Special keys: Space, Enter, Escape, Tab, Backspace, Delete, Insert,
                    Home, End, Page Up, Page Down, Arrow keys
    - Numpad: Num 0-9, Num +, Num -, Num *, Num /
    - Symbols: + - . , [ ] ; / ` \ '
  ═══════════════════════════════════════════════════════════════════════════ }
function TextToKey(const Text: string; out Key: Word; out Shift: TShiftState): Boolean;
var
  S: string;
  KeyPart: string;
begin
  Result := False;
  Key := 0;
  Shift := [];
  S := UpperCase(Trim(Text));

  if (S = '') or (S = '(NONE)') then
  begin
    Result := True;
    Exit;
  end;

  { Parse modifiers }
  while Pos('CTRL+', S) = 1 do
  begin
    Include(Shift, ssCtrl);
    Delete(S, 1, 5);
  end;
  while Pos('ALT+', S) = 1 do
  begin
    Include(Shift, ssAlt);
    Delete(S, 1, 4);
  end;
  while Pos('SHIFT+', S) = 1 do
  begin
    Include(Shift, ssShift);
    Delete(S, 1, 6);
  end;

  KeyPart := S;

  { Parse key }
  if KeyPart = 'SPACE' then Key := VK_SPACE
  else if KeyPart = 'ENTER' then Key := VK_RETURN
  else if KeyPart = 'ESCAPE' then Key := VK_ESCAPE
  else if KeyPart = 'TAB' then Key := VK_TAB
  else if KeyPart = 'BACKSPACE' then Key := VK_BACK
  else if KeyPart = 'DELETE' then Key := VK_DELETE
  else if KeyPart = 'INSERT' then Key := VK_INSERT
  else if KeyPart = 'HOME' then Key := VK_HOME
  else if KeyPart = 'END' then Key := VK_END
  else if KeyPart = 'PAGE UP' then Key := VK_PRIOR
  else if KeyPart = 'PAGE DOWN' then Key := VK_NEXT
  else if KeyPart = 'LEFT' then Key := VK_LEFT
  else if KeyPart = 'RIGHT' then Key := VK_RIGHT
  else if KeyPart = 'UP' then Key := VK_UP
  else if KeyPart = 'DOWN' then Key := VK_DOWN
  else if (Length(KeyPart) = 2) and (KeyPart[1] = 'F') and (KeyPart[2] in ['1'..'9']) then
    Key := VK_F1 + Ord(KeyPart[2]) - Ord('1')
  else if (Length(KeyPart) = 3) and (KeyPart[1] = 'F') and (KeyPart[2] = '1') and (KeyPart[3] in ['0'..'2']) then
    Key := VK_F10 + Ord(KeyPart[3]) - Ord('0')
  else if (Length(KeyPart) >= 4) and (Copy(KeyPart, 1, 4) = 'NUM ') then
  begin
    Delete(KeyPart, 1, 4);
    if KeyPart = '+' then Key := VK_ADD
    else if KeyPart = '-' then Key := VK_SUBTRACT
    else if KeyPart = '*' then Key := VK_MULTIPLY
    else if KeyPart = '/' then Key := VK_DIVIDE
    else if (Length(KeyPart) = 1) and (KeyPart[1] in ['0'..'9']) then
      Key := VK_NUMPAD0 + Ord(KeyPart[1]) - Ord('0');
  end
  else if KeyPart = '+' then Key := VK_OEM_PLUS
  else if KeyPart = '-' then Key := VK_OEM_MINUS
  else if KeyPart = '.' then Key := VK_OEM_PERIOD
  else if KeyPart = ',' then Key := VK_OEM_COMMA
  else if KeyPart = '[' then Key := VK_OEM_4
  else if KeyPart = ']' then Key := VK_OEM_6
  else if KeyPart = ';' then Key := VK_OEM_1
  else if KeyPart = '/' then Key := VK_OEM_2
  else if KeyPart = '`' then Key := VK_OEM_3
  else if KeyPart = '\' then Key := VK_OEM_5
  else if KeyPart = '''' then Key := VK_OEM_7
  else if (Length(KeyPart) = 1) and (KeyPart[1] in ['A'..'Z', '0'..'9']) then
    Key := Ord(KeyPart[1]);

  Result := (Key <> 0) or (Shift = []);
end;

{ TShortcutManager }

constructor TShortcutManager.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  InitializeDefaults;
  Load;
end;

destructor TShortcutManager.Destroy;
begin
  inherited Destroy;
end;

procedure TShortcutManager.InitializeDefaults;
var
  A: TShortcutAction;
begin
  { Initialize all to empty first }
  for A := Low(TShortcutAction) to High(TShortcutAction) do
  begin
    FShortcuts[A].Action := A;
    FShortcuts[A].Key := 0;
    FShortcuts[A].Shift := [];
    FShortcuts[A].DefaultKey := 0;
    FShortcuts[A].DefaultShift := [];
  end;

  { Playback defaults }
  FShortcuts[saPlayPause].DefaultKey := VK_SPACE;
  FShortcuts[saStop].DefaultKey := Ord('S');
  FShortcuts[saPrevious].DefaultKey := Ord('P');
  FShortcuts[saNext].DefaultKey := Ord('N');
  FShortcuts[saSeekForward].DefaultKey := VK_RIGHT;
  FShortcuts[saSeekBackward].DefaultKey := VK_LEFT;
  FShortcuts[saSeekForward5].DefaultKey := VK_RIGHT;
  FShortcuts[saSeekForward5].DefaultShift := [ssCtrl];
  FShortcuts[saSeekBackward5].DefaultKey := VK_LEFT;
  FShortcuts[saSeekBackward5].DefaultShift := [ssCtrl];
  FShortcuts[saSpeedUp].DefaultKey := VK_OEM_6; { ] }
  FShortcuts[saSpeedDown].DefaultKey := VK_OEM_4; { [ }
  FShortcuts[saSpeedReset].DefaultKey := VK_BACK;
  FShortcuts[saSetLoopA].DefaultKey := Ord('L');
  FShortcuts[saSetLoopB].DefaultKey := Ord('L');
  FShortcuts[saSetLoopB].DefaultShift := [ssShift];
  FShortcuts[saClearLoop].DefaultKey := Ord('L');
  FShortcuts[saClearLoop].DefaultShift := [ssCtrl];
  FShortcuts[saFrameForward].DefaultKey := VK_OEM_PERIOD; { . }
  FShortcuts[saFrameBackward].DefaultKey := VK_OEM_COMMA; { , }
  FShortcuts[saPrevChapter].DefaultKey := VK_PRIOR;
  FShortcuts[saNextChapter].DefaultKey := VK_NEXT;
  FShortcuts[saGotoTime].DefaultKey := Ord('G');
  FShortcuts[saGotoTime].DefaultShift := [ssCtrl];

  { Audio defaults }
  FShortcuts[saVolumeUp].DefaultKey := VK_UP;
  FShortcuts[saVolumeDown].DefaultKey := VK_DOWN;
  FShortcuts[saMute].DefaultKey := Ord('M');
  FShortcuts[saMute].DefaultShift := [ssCtrl];
  FShortcuts[saAudioDelayPlus].DefaultKey := VK_OEM_PLUS;
  FShortcuts[saAudioDelayMinus].DefaultKey := VK_OEM_MINUS;
  FShortcuts[saAudioDelayReset].DefaultKey := VK_OEM_PLUS;
  FShortcuts[saAudioDelayReset].DefaultShift := [ssCtrl];

  { Video defaults }
  FShortcuts[saFullscreen].DefaultKey := Ord('F');
  FShortcuts[saRotate].DefaultKey := Ord('R');
  FShortcuts[saDeinterlace].DefaultKey := Ord('D');
  FShortcuts[saZoomIn].DefaultKey := VK_ADD;
  FShortcuts[saZoomOut].DefaultKey := VK_SUBTRACT;
  FShortcuts[saZoomReset].DefaultKey := VK_MULTIPLY;
  FShortcuts[saAlwaysOnTop].DefaultKey := Ord('T');
  FShortcuts[saAlwaysOnTop].DefaultShift := [ssCtrl];
  FShortcuts[saFitToVideo].DefaultKey := Ord('W');
  FShortcuts[saScreenshot].DefaultKey := Ord('S');
  FShortcuts[saScreenshot].DefaultShift := [ssCtrl];
  FShortcuts[saVideoAdjust].DefaultKey := Ord('J');
  FShortcuts[saVideoAdjust].DefaultShift := [ssCtrl];

  { Visualization defaults }
  FShortcuts[saVisNextMode].DefaultKey := Ord('V');
  FShortcuts[saVisNextMode].DefaultShift := [ssCtrl, ssShift];
  FShortcuts[saVisNextColor].DefaultKey := Ord('M');
  FShortcuts[saVisNextColor].DefaultShift := [ssCtrl, ssShift];

  { Subtitle defaults }
  FShortcuts[saSubtitleDelayPlus].DefaultKey := Ord('X');
  FShortcuts[saSubtitleDelayMinus].DefaultKey := Ord('Z');
  FShortcuts[saSubtitleDelayReset].DefaultKey := Ord('Z');
  FShortcuts[saSubtitleDelayReset].DefaultShift := [ssCtrl];

  { DVD Navigation defaults }
  FShortcuts[saDVDMenu].DefaultKey := VK_NUMPAD0;
  FShortcuts[saDVDUp].DefaultKey := VK_NUMPAD8;
  FShortcuts[saDVDDown].DefaultKey := VK_NUMPAD2;
  FShortcuts[saDVDLeft].DefaultKey := VK_NUMPAD4;
  FShortcuts[saDVDRight].DefaultKey := VK_NUMPAD6;
  FShortcuts[saDVDSelect].DefaultKey := VK_NUMPAD5;

  { Window defaults }
  FShortcuts[saOpenFile].DefaultKey := Ord('O');
  FShortcuts[saOpenFile].DefaultShift := [ssCtrl];
  FShortcuts[saOpenURL].DefaultKey := Ord('U');
  FShortcuts[saOpenURL].DefaultShift := [ssCtrl];
  FShortcuts[saPlaylist].DefaultKey := Ord('P');
  FShortcuts[saPlaylist].DefaultShift := [ssCtrl, ssShift];
  FShortcuts[saQuit].DefaultKey := Ord('Q');
  FShortcuts[saQuit].DefaultShift := [ssCtrl];

  { Copy defaults to current }
  for A := Low(TShortcutAction) to High(TShortcutAction) do
  begin
    FShortcuts[A].Key := FShortcuts[A].DefaultKey;
    FShortcuts[A].Shift := FShortcuts[A].DefaultShift;
  end;
end;

function TShortcutManager.GetShortcut(Action: TShortcutAction): TShortcutItem;
begin
  Result := FShortcuts[Action];
end;

procedure TShortcutManager.SetShortcut(Action: TShortcutAction; Key: Word; Shift: TShiftState);
begin
  FShortcuts[Action].Key := Key;
  FShortcuts[Action].Shift := Shift;
end;

procedure TShortcutManager.Load;
var
  Ini: TIniFile;
  A: TShortcutAction;
  S: string;
  Key: Word;
  Shift: TShiftState;
begin
  if not FileExists(FFileName) then Exit;

  Ini := TIniFile.Create(FFileName);
  try
    for A := Low(TShortcutAction) to High(TShortcutAction) do
    begin
      S := Ini.ReadString(SECTION_SHORTCUTS, GetActionName(A), '');
      if S <> '' then
      begin
        if TextToKey(S, Key, Shift) then
          SetShortcut(A, Key, Shift);
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TShortcutManager.Save;
var
  Ini: TIniFile;
  A: TShortcutAction;
begin
  Ini := TIniFile.Create(FFileName);
  try
    for A := Low(TShortcutAction) to High(TShortcutAction) do
    begin
      Ini.WriteString(SECTION_SHORTCUTS, GetActionName(A), ShortcutToText(A));
    end;
  finally
    Ini.Free;
  end;
end;

procedure TShortcutManager.ResetToDefaults;
var
  A: TShortcutAction;
begin
  for A := Low(TShortcutAction) to High(TShortcutAction) do
  begin
    FShortcuts[A].Key := FShortcuts[A].DefaultKey;
    FShortcuts[A].Shift := FShortcuts[A].DefaultShift;
  end;
end;

procedure TShortcutManager.ResetAction(Action: TShortcutAction);
begin
  FShortcuts[Action].Key := FShortcuts[Action].DefaultKey;
  FShortcuts[Action].Shift := FShortcuts[Action].DefaultShift;
end;

{ ═══════════════════════════════════════════════════════════════════════════
  FindAction - Look up the action assigned to a key combination

  Purpose: Searches through all registered shortcuts to find which action
           is bound to a specific key and modifier combination. Used by
           the main form's key handler to dispatch keyboard shortcuts.

  Parameters:
    - Key: The virtual key code pressed (VK_* constant)
    - Shift: The modifier state at time of keypress (Ctrl, Alt, Shift)

  Returns: The TShortcutAction enum value for the matching shortcut.
           Returns saPlayPause as default if no match found.

  Algorithm: O(n) sequential search through all actions in enum order.
             First match wins - duplicate bindings return first occurrence.

  Important: Caller MUST verify the returned action by checking if the key
             actually matches using MatchesShortcut(), since saPlayPause is
             returned both for actual PlayPause shortcut AND as "not found".
  ═══════════════════════════════════════════════════════════════════════════ }
function TShortcutManager.FindAction(Key: Word; Shift: TShiftState): TShortcutAction;
var
  A: TShortcutAction;
begin
  for A := Low(TShortcutAction) to High(TShortcutAction) do
  begin
    if (FShortcuts[A].Key = Key) and (FShortcuts[A].Shift = Shift) then
    begin
      Result := A;
      Exit;
    end;
  end;
  Result := saPlayPause; { Default, caller should check if key matches }
end;

{ ═══════════════════════════════════════════════════════════════════════════
  ActionExists - Check if a key combination is already in use

  Purpose: Validates whether a key/modifier combination is already assigned
           to another action. Used by the shortcut configuration dialog to
           prevent duplicate bindings and warn the user about conflicts.

  Parameters:
    - Key: The virtual key code to check (VK_* constant)
    - Shift: The modifier state to check (Ctrl, Alt, Shift)
    - ExcludeAction: Skip this action when searching (the action being edited)

  Returns: True if the key combination is assigned to any OTHER action.
           False if unassigned or only assigned to ExcludeAction.
           Always returns False for Key=0 (no key assigned).

  Usage Example:
    When user tries to assign Ctrl+S to "Stop", we call:
      ActionExists(Ord('S'), [ssCtrl], saStop)
    If returns True, another action already uses Ctrl+S.

  Algorithm: O(n) sequential search, skipping the excluded action.
  ═══════════════════════════════════════════════════════════════════════════ }
function TShortcutManager.ActionExists(Key: Word; Shift: TShiftState; ExcludeAction: TShortcutAction): Boolean;
var
  A: TShortcutAction;
begin
  Result := False;
  if Key = 0 then Exit;

  for A := Low(TShortcutAction) to High(TShortcutAction) do
  begin
    if A = ExcludeAction then Continue;
    if (FShortcuts[A].Key = Key) and (FShortcuts[A].Shift = Shift) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TShortcutManager.ShortcutToText(Action: TShortcutAction): string;
begin
  Result := KeyToText(FShortcuts[Action].Key, FShortcuts[Action].Shift);
end;

function TShortcutManager.GetActionName(Action: TShortcutAction): string;
begin
  case Action of
    saPlayPause: Result := 'PlayPause';
    saStop: Result := 'Stop';
    saPrevious: Result := 'Previous';
    saNext: Result := 'Next';
    saSeekForward: Result := 'SeekForward';
    saSeekBackward: Result := 'SeekBackward';
    saSeekForward5: Result := 'SeekForward5';
    saSeekBackward5: Result := 'SeekBackward5';
    saSpeedUp: Result := 'SpeedUp';
    saSpeedDown: Result := 'SpeedDown';
    saSpeedReset: Result := 'SpeedReset';
    saSetLoopA: Result := 'SetLoopA';
    saSetLoopB: Result := 'SetLoopB';
    saClearLoop: Result := 'ClearLoop';
    saFrameForward: Result := 'FrameForward';
    saFrameBackward: Result := 'FrameBackward';
    saPrevChapter: Result := 'PrevChapter';
    saNextChapter: Result := 'NextChapter';
    saGotoTime: Result := 'GotoTime';
    saVolumeUp: Result := 'VolumeUp';
    saVolumeDown: Result := 'VolumeDown';
    saMute: Result := 'Mute';
    saAudioDelayPlus: Result := 'AudioDelayPlus';
    saAudioDelayMinus: Result := 'AudioDelayMinus';
    saAudioDelayReset: Result := 'AudioDelayReset';
    saFullscreen: Result := 'Fullscreen';
    saRotate: Result := 'Rotate';
    saDeinterlace: Result := 'Deinterlace';
    saZoomIn: Result := 'ZoomIn';
    saZoomOut: Result := 'ZoomOut';
    saZoomReset: Result := 'ZoomReset';
    saAlwaysOnTop: Result := 'AlwaysOnTop';
    saFitToVideo: Result := 'FitToVideo';
    saScreenshot: Result := 'Screenshot';
    saVideoAdjust: Result := 'VideoAdjust';
    saVisNextMode: Result := 'VisNextMode';
    saVisNextColor: Result := 'VisNextColor';
    saSubtitleDelayPlus: Result := 'SubtitleDelayPlus';
    saSubtitleDelayMinus: Result := 'SubtitleDelayMinus';
    saSubtitleDelayReset: Result := 'SubtitleDelayReset';
    saDVDMenu: Result := 'DVDMenu';
    saDVDUp: Result := 'DVDUp';
    saDVDDown: Result := 'DVDDown';
    saDVDLeft: Result := 'DVDLeft';
    saDVDRight: Result := 'DVDRight';
    saDVDSelect: Result := 'DVDSelect';
    saOpenFile: Result := 'OpenFile';
    saOpenURL: Result := 'OpenURL';
    saPlaylist: Result := 'Playlist';
    saQuit: Result := 'Quit';
  else
    Result := 'Unknown';
  end;
end;

function TShortcutManager.GetActionCategory(Action: TShortcutAction): string;
begin
  case Action of
    saPlayPause..saGotoTime: Result := 'Playback';
    saVolumeUp..saAudioDelayReset: Result := 'Audio';
    saFullscreen..saVideoAdjust: Result := 'Video';
    saVisNextMode..saVisNextColor: Result := 'Visualization';
    saSubtitleDelayPlus..saSubtitleDelayReset: Result := 'Subtitles';
    saDVDMenu..saDVDSelect: Result := 'DVD';
    saOpenFile..saQuit: Result := 'Window';
  else
    Result := 'Other';
  end;
end;

procedure TShortcutManager.AssignShortcut(Action: TShortcutAction; Key: Word; Shift: TShiftState);
begin
  SetShortcut(Action, Key, Shift);
end;

procedure TShortcutManager.ClearShortcut(Action: TShortcutAction);
begin
  SetShortcut(Action, 0, []);
end;

{ Check if a key/shift matches any shortcut }
function TShortcutManager.MatchesShortcut(Key: Word; Shift: TShiftState; Action: TShortcutAction): Boolean;
begin
  Result := (FShortcuts[Action].Key = Key) and (FShortcuts[Action].Shift = Shift);
end;

initialization
  ShortcutManager := TShortcutManager.Create(GetAppDataDir + SHORTCUTS_FILENAME);

finalization
  ShortcutManager.Free;

end.
