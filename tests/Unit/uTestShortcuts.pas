{ ═══════════════════════════════════════════════════════════════════════════════
  uTestShortcuts.pas - Unit Tests for Keyboard Shortcuts Manager

  Part of 3nity Media Test Suite

  Tests for uShortcuts.pas:
  - Helper functions (KeyToText, TextToKey)
  - TShortcutAction enum values
  - TShortcutManager class (creation, defaults, lookup, assignment)
  - Default shortcut bindings
  - Category classification

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestShortcuts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, LCLType,
  uShortcuts;

type
  { ═══════════════════════════════════════════════════════════════════════════
    TTestShortcuts - Test cases for Keyboard Shortcuts
    ═══════════════════════════════════════════════════════════════════════════ }

  TTestShortcuts = class(TTestCase)
  private
    FManager: TShortcutManager;
    FTempFile: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { ─────────────────────────────────────────────────────────────────────────
      KeyToText Tests - Basic Keys
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_KeyToText_Space;
    procedure Test_KeyToText_Enter;
    procedure Test_KeyToText_Escape;
    procedure Test_KeyToText_Tab;
    procedure Test_KeyToText_Backspace;
    procedure Test_KeyToText_Delete;
    procedure Test_KeyToText_Insert;
    procedure Test_KeyToText_Home;
    procedure Test_KeyToText_End;
    procedure Test_KeyToText_PageUp;
    procedure Test_KeyToText_PageDown;
    procedure Test_KeyToText_Left;
    procedure Test_KeyToText_Right;
    procedure Test_KeyToText_Up;
    procedure Test_KeyToText_Down;
    procedure Test_KeyToText_None;

    { ─────────────────────────────────────────────────────────────────────────
      KeyToText Tests - Function Keys
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_KeyToText_F1;
    procedure Test_KeyToText_F5;
    procedure Test_KeyToText_F10;
    procedure Test_KeyToText_F12;

    { ─────────────────────────────────────────────────────────────────────────
      KeyToText Tests - Numpad Keys
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_KeyToText_Numpad0;
    procedure Test_KeyToText_Numpad5;
    procedure Test_KeyToText_Numpad9;
    procedure Test_KeyToText_NumpadAdd;
    procedure Test_KeyToText_NumpadSubtract;
    procedure Test_KeyToText_NumpadMultiply;
    procedure Test_KeyToText_NumpadDivide;

    { ─────────────────────────────────────────────────────────────────────────
      KeyToText Tests - Letters and Numbers
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_KeyToText_LetterA;
    procedure Test_KeyToText_LetterZ;
    procedure Test_KeyToText_Number0;
    procedure Test_KeyToText_Number9;

    { ─────────────────────────────────────────────────────────────────────────
      KeyToText Tests - OEM Keys
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_KeyToText_Plus;
    procedure Test_KeyToText_Minus;
    procedure Test_KeyToText_Period;
    procedure Test_KeyToText_Comma;
    procedure Test_KeyToText_OpenBracket;
    procedure Test_KeyToText_CloseBracket;
    procedure Test_KeyToText_Semicolon;
    procedure Test_KeyToText_Slash;
    procedure Test_KeyToText_Backslash;
    procedure Test_KeyToText_Backtick;
    procedure Test_KeyToText_Quote;

    { ─────────────────────────────────────────────────────────────────────────
      KeyToText Tests - Modifiers
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_KeyToText_CtrlA;
    procedure Test_KeyToText_AltA;
    procedure Test_KeyToText_ShiftA;
    procedure Test_KeyToText_CtrlShiftA;
    procedure Test_KeyToText_CtrlAltA;
    procedure Test_KeyToText_CtrlAltShiftA;

    { ─────────────────────────────────────────────────────────────────────────
      TextToKey Tests - Basic Keys
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TextToKey_Space;
    procedure Test_TextToKey_Enter;
    procedure Test_TextToKey_Escape;
    procedure Test_TextToKey_Tab;
    procedure Test_TextToKey_Backspace;
    procedure Test_TextToKey_Delete;
    procedure Test_TextToKey_Insert;
    procedure Test_TextToKey_Home;
    procedure Test_TextToKey_End;
    procedure Test_TextToKey_PageUp;
    procedure Test_TextToKey_PageDown;
    procedure Test_TextToKey_ArrowKeys;

    { ─────────────────────────────────────────────────────────────────────────
      TextToKey Tests - Function Keys
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TextToKey_F1;
    procedure Test_TextToKey_F9;
    procedure Test_TextToKey_F10;
    procedure Test_TextToKey_F11;
    procedure Test_TextToKey_F12;

    { ─────────────────────────────────────────────────────────────────────────
      TextToKey Tests - Numpad
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TextToKey_Numpad0;
    procedure Test_TextToKey_Numpad9;
    procedure Test_TextToKey_NumpadPlus;
    procedure Test_TextToKey_NumpadMinus;
    procedure Test_TextToKey_NumpadMultiply;
    procedure Test_TextToKey_NumpadDivide;

    { ─────────────────────────────────────────────────────────────────────────
      TextToKey Tests - Letters and Symbols
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TextToKey_LetterA;
    procedure Test_TextToKey_LetterZ;
    procedure Test_TextToKey_Number5;
    procedure Test_TextToKey_Plus;
    procedure Test_TextToKey_Minus;
    procedure Test_TextToKey_Brackets;

    { ─────────────────────────────────────────────────────────────────────────
      TextToKey Tests - Modifiers
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TextToKey_CtrlA;
    procedure Test_TextToKey_AltA;
    procedure Test_TextToKey_ShiftA;
    procedure Test_TextToKey_CtrlShiftA;
    procedure Test_TextToKey_CtrlAltShiftF1;

    { ─────────────────────────────────────────────────────────────────────────
      TextToKey Tests - Special Cases
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TextToKey_Empty;
    procedure Test_TextToKey_None;
    procedure Test_TextToKey_CaseInsensitive;
    procedure Test_TextToKey_Unknown;

    { ─────────────────────────────────────────────────────────────────────────
      TextToKey/KeyToText Roundtrip Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Roundtrip_Space;
    procedure Test_Roundtrip_CtrlShiftF5;
    procedure Test_Roundtrip_AltM;

    { ─────────────────────────────────────────────────────────────────────────
      TShortcutAction Enum Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_TShortcutAction_Count;
    procedure Test_TShortcutAction_First;
    procedure Test_TShortcutAction_Last;

    { ─────────────────────────────────────────────────────────────────────────
      TShortcutManager Creation Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Create_NotNil;
    procedure Test_Create_FileName;

    { ─────────────────────────────────────────────────────────────────────────
      TShortcutManager Default Shortcut Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Default_PlayPause;
    procedure Test_Default_Stop;
    procedure Test_Default_Previous;
    procedure Test_Default_Next;
    procedure Test_Default_SeekForward;
    procedure Test_Default_SeekBackward;
    procedure Test_Default_SeekForward5;
    procedure Test_Default_SeekBackward5;
    procedure Test_Default_SpeedUp;
    procedure Test_Default_SpeedDown;
    procedure Test_Default_SpeedReset;
    procedure Test_Default_VolumeUp;
    procedure Test_Default_VolumeDown;
    procedure Test_Default_Mute;
    procedure Test_Default_Fullscreen;
    procedure Test_Default_OpenFile;
    procedure Test_Default_Quit;

    { ─────────────────────────────────────────────────────────────────────────
      GetActionName Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetActionName_PlayPause;
    procedure Test_GetActionName_Stop;
    procedure Test_GetActionName_VolumeUp;
    procedure Test_GetActionName_Fullscreen;
    procedure Test_GetActionName_OpenFile;
    procedure Test_GetActionName_Quit;

    { ─────────────────────────────────────────────────────────────────────────
      GetActionCategory Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_GetActionCategory_Playback;
    procedure Test_GetActionCategory_Audio;
    procedure Test_GetActionCategory_Video;
    procedure Test_GetActionCategory_Visualization;
    procedure Test_GetActionCategory_Subtitles;
    procedure Test_GetActionCategory_DVD;
    procedure Test_GetActionCategory_Window;

    { ─────────────────────────────────────────────────────────────────────────
      ShortcutToText Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_ShortcutToText_PlayPause;
    procedure Test_ShortcutToText_SeekForward5;
    procedure Test_ShortcutToText_OpenFile;

    { ─────────────────────────────────────────────────────────────────────────
      FindAction Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_FindAction_Space;
    procedure Test_FindAction_CtrlO;
    procedure Test_FindAction_CtrlQ;
    procedure Test_FindAction_NotFound;

    { ─────────────────────────────────────────────────────────────────────────
      MatchesShortcut Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_MatchesShortcut_True;
    procedure Test_MatchesShortcut_False_Key;
    procedure Test_MatchesShortcut_False_Shift;

    { ─────────────────────────────────────────────────────────────────────────
      ActionExists Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_ActionExists_True;
    procedure Test_ActionExists_False;
    procedure Test_ActionExists_ExcludesSelf;
    procedure Test_ActionExists_ZeroKey;

    { ─────────────────────────────────────────────────────────────────────────
      AssignShortcut Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_AssignShortcut_NewKey;
    procedure Test_AssignShortcut_WithShift;

    { ─────────────────────────────────────────────────────────────────────────
      ClearShortcut Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_ClearShortcut_ClearsKey;
    procedure Test_ClearShortcut_ClearsShift;

    { ─────────────────────────────────────────────────────────────────────────
      ResetAction Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_ResetAction_RestoresDefault;

    { ─────────────────────────────────────────────────────────────────────────
      ResetToDefaults Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_ResetToDefaults_RestoresAll;

    { ─────────────────────────────────────────────────────────────────────────
      Shortcuts Property Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Shortcuts_ReturnsCorrectAction;
    procedure Test_Shortcuts_ReturnsCorrectKey;
    procedure Test_Shortcuts_ReturnsCorrectShift;
    procedure Test_Shortcuts_ReturnsDefaultKey;
    procedure Test_Shortcuts_ReturnsDefaultShift;

    { ─────────────────────────────────────────────────────────────────────────
      Save/Load Tests
      ───────────────────────────────────────────────────────────────────────── }
    procedure Test_Save_CreatesFile;
    procedure Test_Load_RestoresShortcuts;
  end;

implementation

uses
  uConfig;

{ ═══════════════════════════════════════════════════════════════════════════════
  TTestShortcuts Implementation
  ═══════════════════════════════════════════════════════════════════════════════ }

procedure TTestShortcuts.SetUp;
begin
  FTempFile := GetTempDir + 'test_shortcuts_' + IntToStr(Random(100000)) + '.ini';
  FManager := TShortcutManager.Create(FTempFile);
end;

procedure TTestShortcuts.TearDown;
begin
  FManager.Free;
  FManager := nil;
  if FileExists(FTempFile) then
    DeleteFile(FTempFile);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  KeyToText Tests - Basic Keys
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_KeyToText_Space;
begin
  AssertEquals('Space', 'Space', KeyToText(VK_SPACE, []));
end;

procedure TTestShortcuts.Test_KeyToText_Enter;
begin
  AssertEquals('Enter', 'Enter', KeyToText(VK_RETURN, []));
end;

procedure TTestShortcuts.Test_KeyToText_Escape;
begin
  AssertEquals('Escape', 'Escape', KeyToText(VK_ESCAPE, []));
end;

procedure TTestShortcuts.Test_KeyToText_Tab;
begin
  AssertEquals('Tab', 'Tab', KeyToText(VK_TAB, []));
end;

procedure TTestShortcuts.Test_KeyToText_Backspace;
begin
  AssertEquals('Backspace', 'Backspace', KeyToText(VK_BACK, []));
end;

procedure TTestShortcuts.Test_KeyToText_Delete;
begin
  AssertEquals('Delete', 'Delete', KeyToText(VK_DELETE, []));
end;

procedure TTestShortcuts.Test_KeyToText_Insert;
begin
  AssertEquals('Insert', 'Insert', KeyToText(VK_INSERT, []));
end;

procedure TTestShortcuts.Test_KeyToText_Home;
begin
  AssertEquals('Home', 'Home', KeyToText(VK_HOME, []));
end;

procedure TTestShortcuts.Test_KeyToText_End;
begin
  AssertEquals('End', 'End', KeyToText(VK_END, []));
end;

procedure TTestShortcuts.Test_KeyToText_PageUp;
begin
  AssertEquals('Page Up', 'Page Up', KeyToText(VK_PRIOR, []));
end;

procedure TTestShortcuts.Test_KeyToText_PageDown;
begin
  AssertEquals('Page Down', 'Page Down', KeyToText(VK_NEXT, []));
end;

procedure TTestShortcuts.Test_KeyToText_Left;
begin
  AssertEquals('Left', 'Left', KeyToText(VK_LEFT, []));
end;

procedure TTestShortcuts.Test_KeyToText_Right;
begin
  AssertEquals('Right', 'Right', KeyToText(VK_RIGHT, []));
end;

procedure TTestShortcuts.Test_KeyToText_Up;
begin
  AssertEquals('Up', 'Up', KeyToText(VK_UP, []));
end;

procedure TTestShortcuts.Test_KeyToText_Down;
begin
  AssertEquals('Down', 'Down', KeyToText(VK_DOWN, []));
end;

procedure TTestShortcuts.Test_KeyToText_None;
begin
  AssertEquals('None', '(None)', KeyToText(0, []));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  KeyToText Tests - Function Keys
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_KeyToText_F1;
begin
  AssertEquals('F1', 'F1', KeyToText(VK_F1, []));
end;

procedure TTestShortcuts.Test_KeyToText_F5;
begin
  AssertEquals('F5', 'F5', KeyToText(VK_F5, []));
end;

procedure TTestShortcuts.Test_KeyToText_F10;
begin
  AssertEquals('F10', 'F10', KeyToText(VK_F10, []));
end;

procedure TTestShortcuts.Test_KeyToText_F12;
begin
  AssertEquals('F12', 'F12', KeyToText(VK_F12, []));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  KeyToText Tests - Numpad Keys
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_KeyToText_Numpad0;
begin
  AssertEquals('Num 0', 'Num 0', KeyToText(VK_NUMPAD0, []));
end;

procedure TTestShortcuts.Test_KeyToText_Numpad5;
begin
  AssertEquals('Num 5', 'Num 5', KeyToText(VK_NUMPAD5, []));
end;

procedure TTestShortcuts.Test_KeyToText_Numpad9;
begin
  AssertEquals('Num 9', 'Num 9', KeyToText(VK_NUMPAD9, []));
end;

procedure TTestShortcuts.Test_KeyToText_NumpadAdd;
begin
  AssertEquals('Num +', 'Num +', KeyToText(VK_ADD, []));
end;

procedure TTestShortcuts.Test_KeyToText_NumpadSubtract;
begin
  AssertEquals('Num -', 'Num -', KeyToText(VK_SUBTRACT, []));
end;

procedure TTestShortcuts.Test_KeyToText_NumpadMultiply;
begin
  AssertEquals('Num *', 'Num *', KeyToText(VK_MULTIPLY, []));
end;

procedure TTestShortcuts.Test_KeyToText_NumpadDivide;
begin
  AssertEquals('Num /', 'Num /', KeyToText(VK_DIVIDE, []));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  KeyToText Tests - Letters and Numbers
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_KeyToText_LetterA;
begin
  AssertEquals('A', 'A', KeyToText(Ord('A'), []));
end;

procedure TTestShortcuts.Test_KeyToText_LetterZ;
begin
  AssertEquals('Z', 'Z', KeyToText(Ord('Z'), []));
end;

procedure TTestShortcuts.Test_KeyToText_Number0;
begin
  AssertEquals('0', '0', KeyToText(Ord('0'), []));
end;

procedure TTestShortcuts.Test_KeyToText_Number9;
begin
  AssertEquals('9', '9', KeyToText(Ord('9'), []));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  KeyToText Tests - OEM Keys
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_KeyToText_Plus;
begin
  AssertEquals('+', '+', KeyToText(VK_OEM_PLUS, []));
end;

procedure TTestShortcuts.Test_KeyToText_Minus;
begin
  AssertEquals('-', '-', KeyToText(VK_OEM_MINUS, []));
end;

procedure TTestShortcuts.Test_KeyToText_Period;
begin
  AssertEquals('.', '.', KeyToText(VK_OEM_PERIOD, []));
end;

procedure TTestShortcuts.Test_KeyToText_Comma;
begin
  AssertEquals(',', ',', KeyToText(VK_OEM_COMMA, []));
end;

procedure TTestShortcuts.Test_KeyToText_OpenBracket;
begin
  AssertEquals('[', '[', KeyToText(VK_OEM_4, []));
end;

procedure TTestShortcuts.Test_KeyToText_CloseBracket;
begin
  AssertEquals(']', ']', KeyToText(VK_OEM_6, []));
end;

procedure TTestShortcuts.Test_KeyToText_Semicolon;
begin
  AssertEquals(';', ';', KeyToText(VK_OEM_1, []));
end;

procedure TTestShortcuts.Test_KeyToText_Slash;
begin
  AssertEquals('/', '/', KeyToText(VK_OEM_2, []));
end;

procedure TTestShortcuts.Test_KeyToText_Backslash;
begin
  AssertEquals('\', '\', KeyToText(VK_OEM_5, []));
end;

procedure TTestShortcuts.Test_KeyToText_Backtick;
begin
  AssertEquals('`', '`', KeyToText(VK_OEM_3, []));
end;

procedure TTestShortcuts.Test_KeyToText_Quote;
begin
  AssertEquals('''', '''', KeyToText(VK_OEM_7, []));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  KeyToText Tests - Modifiers
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_KeyToText_CtrlA;
begin
  AssertEquals('Ctrl+A', 'Ctrl+A', KeyToText(Ord('A'), [ssCtrl]));
end;

procedure TTestShortcuts.Test_KeyToText_AltA;
begin
  AssertEquals('Alt+A', 'Alt+A', KeyToText(Ord('A'), [ssAlt]));
end;

procedure TTestShortcuts.Test_KeyToText_ShiftA;
begin
  AssertEquals('Shift+A', 'Shift+A', KeyToText(Ord('A'), [ssShift]));
end;

procedure TTestShortcuts.Test_KeyToText_CtrlShiftA;
begin
  AssertEquals('Ctrl+Shift+A', 'Ctrl+Shift+A', KeyToText(Ord('A'), [ssCtrl, ssShift]));
end;

procedure TTestShortcuts.Test_KeyToText_CtrlAltA;
begin
  AssertEquals('Ctrl+Alt+A', 'Ctrl+Alt+A', KeyToText(Ord('A'), [ssCtrl, ssAlt]));
end;

procedure TTestShortcuts.Test_KeyToText_CtrlAltShiftA;
begin
  AssertEquals('Ctrl+Alt+Shift+A', 'Ctrl+Alt+Shift+A', KeyToText(Ord('A'), [ssCtrl, ssAlt, ssShift]));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TextToKey Tests - Basic Keys
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_TextToKey_Space;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Space', Key, Shift));
  AssertEquals('Key', VK_SPACE, Key);
  AssertTrue('Shift empty', Shift = []);
end;

procedure TTestShortcuts.Test_TextToKey_Enter;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Enter', Key, Shift));
  AssertEquals('Key', VK_RETURN, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Escape;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Escape', Key, Shift));
  AssertEquals('Key', VK_ESCAPE, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Tab;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Tab', Key, Shift));
  AssertEquals('Key', VK_TAB, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Backspace;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Backspace', Key, Shift));
  AssertEquals('Key', VK_BACK, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Delete;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Delete', Key, Shift));
  AssertEquals('Key', VK_DELETE, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Insert;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Insert', Key, Shift));
  AssertEquals('Key', VK_INSERT, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Home;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Home', Key, Shift));
  AssertEquals('Key', VK_HOME, Key);
end;

procedure TTestShortcuts.Test_TextToKey_End;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('End', Key, Shift));
  AssertEquals('Key', VK_END, Key);
end;

procedure TTestShortcuts.Test_TextToKey_PageUp;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Page Up', Key, Shift));
  AssertEquals('Key', VK_PRIOR, Key);
end;

procedure TTestShortcuts.Test_TextToKey_PageDown;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Page Down', Key, Shift));
  AssertEquals('Key', VK_NEXT, Key);
end;

procedure TTestShortcuts.Test_TextToKey_ArrowKeys;
var
  Key: Word;
  Shift: TShiftState;
begin
  TextToKey('Left', Key, Shift);
  AssertEquals('Left', VK_LEFT, Key);
  TextToKey('Right', Key, Shift);
  AssertEquals('Right', VK_RIGHT, Key);
  TextToKey('Up', Key, Shift);
  AssertEquals('Up', VK_UP, Key);
  TextToKey('Down', Key, Shift);
  AssertEquals('Down', VK_DOWN, Key);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TextToKey Tests - Function Keys
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_TextToKey_F1;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('F1', Key, Shift));
  AssertEquals('Key', VK_F1, Key);
end;

procedure TTestShortcuts.Test_TextToKey_F9;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('F9', Key, Shift));
  AssertEquals('Key', VK_F9, Key);
end;

procedure TTestShortcuts.Test_TextToKey_F10;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('F10', Key, Shift));
  AssertEquals('Key', VK_F10, Key);
end;

procedure TTestShortcuts.Test_TextToKey_F11;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('F11', Key, Shift));
  AssertEquals('Key', VK_F11, Key);
end;

procedure TTestShortcuts.Test_TextToKey_F12;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('F12', Key, Shift));
  AssertEquals('Key', VK_F12, Key);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TextToKey Tests - Numpad
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_TextToKey_Numpad0;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Num 0', Key, Shift));
  AssertEquals('Key', VK_NUMPAD0, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Numpad9;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Num 9', Key, Shift));
  AssertEquals('Key', VK_NUMPAD9, Key);
end;

procedure TTestShortcuts.Test_TextToKey_NumpadPlus;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Num +', Key, Shift));
  AssertEquals('Key', VK_ADD, Key);
end;

procedure TTestShortcuts.Test_TextToKey_NumpadMinus;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Num -', Key, Shift));
  AssertEquals('Key', VK_SUBTRACT, Key);
end;

procedure TTestShortcuts.Test_TextToKey_NumpadMultiply;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Num *', Key, Shift));
  AssertEquals('Key', VK_MULTIPLY, Key);
end;

procedure TTestShortcuts.Test_TextToKey_NumpadDivide;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Num /', Key, Shift));
  AssertEquals('Key', VK_DIVIDE, Key);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TextToKey Tests - Letters and Symbols
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_TextToKey_LetterA;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('A', Key, Shift));
  AssertEquals('Key', Ord('A'), Key);
end;

procedure TTestShortcuts.Test_TextToKey_LetterZ;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Z', Key, Shift));
  AssertEquals('Key', Ord('Z'), Key);
end;

procedure TTestShortcuts.Test_TextToKey_Number5;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('5', Key, Shift));
  AssertEquals('Key', Ord('5'), Key);
end;

procedure TTestShortcuts.Test_TextToKey_Plus;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('+', Key, Shift));
  AssertEquals('Key', VK_OEM_PLUS, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Minus;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('-', Key, Shift));
  AssertEquals('Key', VK_OEM_MINUS, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Brackets;
var
  Key: Word;
  Shift: TShiftState;
begin
  TextToKey('[', Key, Shift);
  AssertEquals('[', VK_OEM_4, Key);
  TextToKey(']', Key, Shift);
  AssertEquals(']', VK_OEM_6, Key);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TextToKey Tests - Modifiers
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_TextToKey_CtrlA;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Ctrl+A', Key, Shift));
  AssertEquals('Key', Ord('A'), Key);
  AssertTrue('Has Ctrl', ssCtrl in Shift);
end;

procedure TTestShortcuts.Test_TextToKey_AltA;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Alt+A', Key, Shift));
  AssertEquals('Key', Ord('A'), Key);
  AssertTrue('Has Alt', ssAlt in Shift);
end;

procedure TTestShortcuts.Test_TextToKey_ShiftA;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Shift+A', Key, Shift));
  AssertEquals('Key', Ord('A'), Key);
  AssertTrue('Has Shift', ssShift in Shift);
end;

procedure TTestShortcuts.Test_TextToKey_CtrlShiftA;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Ctrl+Shift+A', Key, Shift));
  AssertEquals('Key', Ord('A'), Key);
  AssertTrue('Has Ctrl', ssCtrl in Shift);
  AssertTrue('Has Shift', ssShift in Shift);
end;

procedure TTestShortcuts.Test_TextToKey_CtrlAltShiftF1;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('Ctrl+Alt+Shift+F1', Key, Shift));
  AssertEquals('Key', VK_F1, Key);
  AssertTrue('Has Ctrl', ssCtrl in Shift);
  AssertTrue('Has Alt', ssAlt in Shift);
  AssertTrue('Has Shift', ssShift in Shift);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TextToKey Tests - Special Cases
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_TextToKey_Empty;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('', Key, Shift));
  AssertEquals('Key is 0', 0, Key);
end;

procedure TTestShortcuts.Test_TextToKey_None;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse', TextToKey('(None)', Key, Shift));
  AssertEquals('Key is 0', 0, Key);
end;

procedure TTestShortcuts.Test_TextToKey_CaseInsensitive;
var
  Key: Word;
  Shift: TShiftState;
begin
  AssertTrue('Should parse lowercase', TextToKey('space', Key, Shift));
  AssertEquals('Key', VK_SPACE, Key);
  AssertTrue('Should parse mixed', TextToKey('SpAcE', Key, Shift));
  AssertEquals('Key', VK_SPACE, Key);
end;

procedure TTestShortcuts.Test_TextToKey_Unknown;
var
  Key: Word;
  Shift: TShiftState;
  R: Boolean;
begin
  R := TextToKey('UnknownKey', Key, Shift);
  { TextToKey returns True with Key=0 for unrecognized input without modifiers }
  { The function is lenient - it only fails if Key=0 AND there were modifiers }
  AssertEquals('Key is 0 for unknown', 0, Key);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TextToKey/KeyToText Roundtrip Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_Roundtrip_Space;
var
  Key: Word;
  Shift: TShiftState;
  Text: string;
begin
  Text := KeyToText(VK_SPACE, []);
  TextToKey(Text, Key, Shift);
  AssertEquals('Roundtrip Space', VK_SPACE, Key);
end;

procedure TTestShortcuts.Test_Roundtrip_CtrlShiftF5;
var
  Key: Word;
  Shift: TShiftState;
  Text: string;
begin
  Text := KeyToText(VK_F5, [ssCtrl, ssShift]);
  TextToKey(Text, Key, Shift);
  AssertEquals('Key', VK_F5, Key);
  AssertTrue('Has Ctrl', ssCtrl in Shift);
  AssertTrue('Has Shift', ssShift in Shift);
end;

procedure TTestShortcuts.Test_Roundtrip_AltM;
var
  Key: Word;
  Shift: TShiftState;
  Text: string;
begin
  Text := KeyToText(Ord('M'), [ssAlt]);
  TextToKey(Text, Key, Shift);
  AssertEquals('Key', Ord('M'), Key);
  AssertTrue('Has Alt', ssAlt in Shift);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TShortcutAction Enum Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_TShortcutAction_Count;
begin
  { TShortcutAction has 50 actions from saPlayPause to saQuit }
  AssertEquals('TShortcutAction count', 50, Ord(High(TShortcutAction)) - Ord(Low(TShortcutAction)) + 1);
end;

procedure TTestShortcuts.Test_TShortcutAction_First;
begin
  AssertEquals('First action', Ord(saPlayPause), Ord(Low(TShortcutAction)));
end;

procedure TTestShortcuts.Test_TShortcutAction_Last;
begin
  AssertEquals('Last action', Ord(saQuit), Ord(High(TShortcutAction)));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TShortcutManager Creation Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_Create_NotNil;
begin
  AssertNotNull('Manager should exist', FManager);
end;

procedure TTestShortcuts.Test_Create_FileName;
begin
  AssertEquals('FileName', FTempFile, FManager.FileName);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TShortcutManager Default Shortcut Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_Default_PlayPause;
begin
  AssertEquals('PlayPause key', VK_SPACE, FManager.Shortcuts[saPlayPause].Key);
  AssertTrue('PlayPause shift', FManager.Shortcuts[saPlayPause].Shift = []);
end;

procedure TTestShortcuts.Test_Default_Stop;
begin
  AssertEquals('Stop key', Ord('S'), FManager.Shortcuts[saStop].Key);
end;

procedure TTestShortcuts.Test_Default_Previous;
begin
  AssertEquals('Previous key', Ord('P'), FManager.Shortcuts[saPrevious].Key);
end;

procedure TTestShortcuts.Test_Default_Next;
begin
  AssertEquals('Next key', Ord('N'), FManager.Shortcuts[saNext].Key);
end;

procedure TTestShortcuts.Test_Default_SeekForward;
begin
  AssertEquals('SeekForward key', VK_RIGHT, FManager.Shortcuts[saSeekForward].Key);
  AssertTrue('SeekForward no shift', FManager.Shortcuts[saSeekForward].Shift = []);
end;

procedure TTestShortcuts.Test_Default_SeekBackward;
begin
  AssertEquals('SeekBackward key', VK_LEFT, FManager.Shortcuts[saSeekBackward].Key);
end;

procedure TTestShortcuts.Test_Default_SeekForward5;
begin
  AssertEquals('SeekForward5 key', VK_RIGHT, FManager.Shortcuts[saSeekForward5].Key);
  AssertTrue('SeekForward5 has Ctrl', ssCtrl in FManager.Shortcuts[saSeekForward5].Shift);
end;

procedure TTestShortcuts.Test_Default_SeekBackward5;
begin
  AssertEquals('SeekBackward5 key', VK_LEFT, FManager.Shortcuts[saSeekBackward5].Key);
  AssertTrue('SeekBackward5 has Ctrl', ssCtrl in FManager.Shortcuts[saSeekBackward5].Shift);
end;

procedure TTestShortcuts.Test_Default_SpeedUp;
begin
  AssertEquals('SpeedUp key', VK_OEM_6, FManager.Shortcuts[saSpeedUp].Key);
end;

procedure TTestShortcuts.Test_Default_SpeedDown;
begin
  AssertEquals('SpeedDown key', VK_OEM_4, FManager.Shortcuts[saSpeedDown].Key);
end;

procedure TTestShortcuts.Test_Default_SpeedReset;
begin
  AssertEquals('SpeedReset key', VK_BACK, FManager.Shortcuts[saSpeedReset].Key);
end;

procedure TTestShortcuts.Test_Default_VolumeUp;
begin
  AssertEquals('VolumeUp key', VK_UP, FManager.Shortcuts[saVolumeUp].Key);
end;

procedure TTestShortcuts.Test_Default_VolumeDown;
begin
  AssertEquals('VolumeDown key', VK_DOWN, FManager.Shortcuts[saVolumeDown].Key);
end;

procedure TTestShortcuts.Test_Default_Mute;
begin
  AssertEquals('Mute key', Ord('M'), FManager.Shortcuts[saMute].Key);
  AssertTrue('Mute has Ctrl', ssCtrl in FManager.Shortcuts[saMute].Shift);
end;

procedure TTestShortcuts.Test_Default_Fullscreen;
begin
  AssertEquals('Fullscreen key', Ord('F'), FManager.Shortcuts[saFullscreen].Key);
end;

procedure TTestShortcuts.Test_Default_OpenFile;
begin
  AssertEquals('OpenFile key', Ord('O'), FManager.Shortcuts[saOpenFile].Key);
  AssertTrue('OpenFile has Ctrl', ssCtrl in FManager.Shortcuts[saOpenFile].Shift);
end;

procedure TTestShortcuts.Test_Default_Quit;
begin
  AssertEquals('Quit key', Ord('Q'), FManager.Shortcuts[saQuit].Key);
  AssertTrue('Quit has Ctrl', ssCtrl in FManager.Shortcuts[saQuit].Shift);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  GetActionName Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_GetActionName_PlayPause;
begin
  AssertEquals('PlayPause', 'PlayPause', FManager.GetActionName(saPlayPause));
end;

procedure TTestShortcuts.Test_GetActionName_Stop;
begin
  AssertEquals('Stop', 'Stop', FManager.GetActionName(saStop));
end;

procedure TTestShortcuts.Test_GetActionName_VolumeUp;
begin
  AssertEquals('VolumeUp', 'VolumeUp', FManager.GetActionName(saVolumeUp));
end;

procedure TTestShortcuts.Test_GetActionName_Fullscreen;
begin
  AssertEquals('Fullscreen', 'Fullscreen', FManager.GetActionName(saFullscreen));
end;

procedure TTestShortcuts.Test_GetActionName_OpenFile;
begin
  AssertEquals('OpenFile', 'OpenFile', FManager.GetActionName(saOpenFile));
end;

procedure TTestShortcuts.Test_GetActionName_Quit;
begin
  AssertEquals('Quit', 'Quit', FManager.GetActionName(saQuit));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  GetActionCategory Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_GetActionCategory_Playback;
begin
  AssertEquals('PlayPause category', 'Playback', FManager.GetActionCategory(saPlayPause));
  AssertEquals('Stop category', 'Playback', FManager.GetActionCategory(saStop));
  AssertEquals('GotoTime category', 'Playback', FManager.GetActionCategory(saGotoTime));
end;

procedure TTestShortcuts.Test_GetActionCategory_Audio;
begin
  AssertEquals('VolumeUp category', 'Audio', FManager.GetActionCategory(saVolumeUp));
  AssertEquals('Mute category', 'Audio', FManager.GetActionCategory(saMute));
  AssertEquals('AudioDelayReset category', 'Audio', FManager.GetActionCategory(saAudioDelayReset));
end;

procedure TTestShortcuts.Test_GetActionCategory_Video;
begin
  AssertEquals('Fullscreen category', 'Video', FManager.GetActionCategory(saFullscreen));
  AssertEquals('Rotate category', 'Video', FManager.GetActionCategory(saRotate));
  AssertEquals('VideoAdjust category', 'Video', FManager.GetActionCategory(saVideoAdjust));
end;

procedure TTestShortcuts.Test_GetActionCategory_Visualization;
begin
  AssertEquals('VisNextMode category', 'Visualization', FManager.GetActionCategory(saVisNextMode));
  AssertEquals('VisNextColor category', 'Visualization', FManager.GetActionCategory(saVisNextColor));
end;

procedure TTestShortcuts.Test_GetActionCategory_Subtitles;
begin
  AssertEquals('SubtitleDelayPlus category', 'Subtitles', FManager.GetActionCategory(saSubtitleDelayPlus));
  AssertEquals('SubtitleDelayReset category', 'Subtitles', FManager.GetActionCategory(saSubtitleDelayReset));
end;

procedure TTestShortcuts.Test_GetActionCategory_DVD;
begin
  AssertEquals('DVDMenu category', 'DVD', FManager.GetActionCategory(saDVDMenu));
  AssertEquals('DVDSelect category', 'DVD', FManager.GetActionCategory(saDVDSelect));
end;

procedure TTestShortcuts.Test_GetActionCategory_Window;
begin
  AssertEquals('OpenFile category', 'Window', FManager.GetActionCategory(saOpenFile));
  AssertEquals('Quit category', 'Window', FManager.GetActionCategory(saQuit));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ShortcutToText Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_ShortcutToText_PlayPause;
begin
  AssertEquals('PlayPause text', 'Space', FManager.ShortcutToText(saPlayPause));
end;

procedure TTestShortcuts.Test_ShortcutToText_SeekForward5;
begin
  AssertEquals('SeekForward5 text', 'Ctrl+Right', FManager.ShortcutToText(saSeekForward5));
end;

procedure TTestShortcuts.Test_ShortcutToText_OpenFile;
begin
  AssertEquals('OpenFile text', 'Ctrl+O', FManager.ShortcutToText(saOpenFile));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  FindAction Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_FindAction_Space;
begin
  AssertEquals('Space finds PlayPause', Ord(saPlayPause), Ord(FManager.FindAction(VK_SPACE, [])));
end;

procedure TTestShortcuts.Test_FindAction_CtrlO;
begin
  AssertEquals('Ctrl+O finds OpenFile', Ord(saOpenFile), Ord(FManager.FindAction(Ord('O'), [ssCtrl])));
end;

procedure TTestShortcuts.Test_FindAction_CtrlQ;
begin
  AssertEquals('Ctrl+Q finds Quit', Ord(saQuit), Ord(FManager.FindAction(Ord('Q'), [ssCtrl])));
end;

procedure TTestShortcuts.Test_FindAction_NotFound;
var
  Action: TShortcutAction;
begin
  { Unassigned key returns saPlayPause as default }
  Action := FManager.FindAction(VK_F11, [ssCtrl, ssAlt, ssShift]);
  { The result is saPlayPause but caller should verify with MatchesShortcut }
  AssertEquals('Returns default', Ord(saPlayPause), Ord(Action));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  MatchesShortcut Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_MatchesShortcut_True;
begin
  AssertTrue('Space matches PlayPause', FManager.MatchesShortcut(VK_SPACE, [], saPlayPause));
end;

procedure TTestShortcuts.Test_MatchesShortcut_False_Key;
begin
  AssertFalse('Enter does not match PlayPause', FManager.MatchesShortcut(VK_RETURN, [], saPlayPause));
end;

procedure TTestShortcuts.Test_MatchesShortcut_False_Shift;
begin
  AssertFalse('Ctrl+Space does not match PlayPause', FManager.MatchesShortcut(VK_SPACE, [ssCtrl], saPlayPause));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ActionExists Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_ActionExists_True;
begin
  { Space is assigned to PlayPause }
  AssertTrue('Space exists for Stop', FManager.ActionExists(VK_SPACE, [], saStop));
end;

procedure TTestShortcuts.Test_ActionExists_False;
begin
  { F11 + Ctrl+Alt+Shift is not assigned }
  AssertFalse('Unassigned key', FManager.ActionExists(VK_F11, [ssCtrl, ssAlt, ssShift], saPlayPause));
end;

procedure TTestShortcuts.Test_ActionExists_ExcludesSelf;
begin
  { Space is assigned to PlayPause, but excluded when checking for PlayPause }
  AssertFalse('Excludes self', FManager.ActionExists(VK_SPACE, [], saPlayPause));
end;

procedure TTestShortcuts.Test_ActionExists_ZeroKey;
begin
  { Key 0 always returns False }
  AssertFalse('Zero key', FManager.ActionExists(0, [], saPlayPause));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  AssignShortcut Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_AssignShortcut_NewKey;
begin
  FManager.AssignShortcut(saPlayPause, VK_F1, []);
  AssertEquals('New key assigned', VK_F1, FManager.Shortcuts[saPlayPause].Key);
end;

procedure TTestShortcuts.Test_AssignShortcut_WithShift;
begin
  FManager.AssignShortcut(saPlayPause, Ord('P'), [ssCtrl, ssShift]);
  AssertEquals('Key assigned', Ord('P'), FManager.Shortcuts[saPlayPause].Key);
  AssertTrue('Has Ctrl', ssCtrl in FManager.Shortcuts[saPlayPause].Shift);
  AssertTrue('Has Shift', ssShift in FManager.Shortcuts[saPlayPause].Shift);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ClearShortcut Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_ClearShortcut_ClearsKey;
begin
  FManager.ClearShortcut(saPlayPause);
  AssertEquals('Key cleared', 0, FManager.Shortcuts[saPlayPause].Key);
end;

procedure TTestShortcuts.Test_ClearShortcut_ClearsShift;
begin
  FManager.AssignShortcut(saPlayPause, VK_F1, [ssCtrl]);
  FManager.ClearShortcut(saPlayPause);
  AssertTrue('Shift cleared', FManager.Shortcuts[saPlayPause].Shift = []);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ResetAction Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_ResetAction_RestoresDefault;
begin
  FManager.AssignShortcut(saPlayPause, VK_F1, [ssCtrl]);
  FManager.ResetAction(saPlayPause);
  AssertEquals('Key restored', VK_SPACE, FManager.Shortcuts[saPlayPause].Key);
  AssertTrue('Shift restored', FManager.Shortcuts[saPlayPause].Shift = []);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ResetToDefaults Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_ResetToDefaults_RestoresAll;
begin
  { Modify several shortcuts }
  FManager.AssignShortcut(saPlayPause, VK_F1, []);
  FManager.AssignShortcut(saStop, VK_F2, [ssCtrl]);
  FManager.ClearShortcut(saNext);
  { Reset all }
  FManager.ResetToDefaults;
  { Verify defaults restored }
  AssertEquals('PlayPause restored', VK_SPACE, FManager.Shortcuts[saPlayPause].Key);
  AssertEquals('Stop restored', Ord('S'), FManager.Shortcuts[saStop].Key);
  AssertEquals('Next restored', Ord('N'), FManager.Shortcuts[saNext].Key);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Shortcuts Property Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_Shortcuts_ReturnsCorrectAction;
var
  Item: TShortcutItem;
begin
  Item := FManager.Shortcuts[saVolumeUp];
  AssertEquals('Action', Ord(saVolumeUp), Ord(Item.Action));
end;

procedure TTestShortcuts.Test_Shortcuts_ReturnsCorrectKey;
var
  Item: TShortcutItem;
begin
  Item := FManager.Shortcuts[saVolumeUp];
  AssertEquals('Key', VK_UP, Item.Key);
end;

procedure TTestShortcuts.Test_Shortcuts_ReturnsCorrectShift;
var
  Item: TShortcutItem;
begin
  Item := FManager.Shortcuts[saOpenFile];
  AssertTrue('Has Ctrl', ssCtrl in Item.Shift);
end;

procedure TTestShortcuts.Test_Shortcuts_ReturnsDefaultKey;
var
  Item: TShortcutItem;
begin
  Item := FManager.Shortcuts[saPlayPause];
  AssertEquals('DefaultKey', VK_SPACE, Item.DefaultKey);
end;

procedure TTestShortcuts.Test_Shortcuts_ReturnsDefaultShift;
var
  Item: TShortcutItem;
begin
  Item := FManager.Shortcuts[saOpenFile];
  AssertTrue('DefaultShift has Ctrl', ssCtrl in Item.DefaultShift);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Save/Load Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestShortcuts.Test_Save_CreatesFile;
begin
  FManager.Save;
  AssertTrue('File created', FileExists(FTempFile));
end;

procedure TTestShortcuts.Test_Load_RestoresShortcuts;
var
  Manager2: TShortcutManager;
begin
  { Modify and save }
  FManager.AssignShortcut(saPlayPause, VK_F5, [ssAlt]);
  FManager.Save;
  { Create new manager and load }
  Manager2 := TShortcutManager.Create(FTempFile);
  try
    AssertEquals('Key loaded', VK_F5, Manager2.Shortcuts[saPlayPause].Key);
    AssertTrue('Shift loaded', ssAlt in Manager2.Shortcuts[saPlayPause].Shift);
  finally
    Manager2.Free;
  end;
end;

initialization
  RegisterTest(TTestShortcuts);

end.
