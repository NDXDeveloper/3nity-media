{ ═══════════════════════════════════════════════════════════════════════════════
  uLibMPV.pas - libmpv C API Headers for Free Pascal/Lazarus

  Part of 3nity Media - Lazarus Edition

  This unit provides Pascal bindings for the libmpv library, enabling
  multimedia playback functionality. It translates the C API from
  client.h to Pascal declarations.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0

  Based on mpv client API version 2.x
  Reference: https://github.com/mpv-player/mpv/blob/master/libmpv/client.h
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uLibMPV;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}
  {$CALLING stdcall}
{$ELSE}
  {$CALLING cdecl}
{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, DynLibs;

const
  {$IFDEF WINDOWS}
  LIBMPV_DLL = 'mpv-2.dll';
  {$ELSE}
    {$IFDEF DARWIN}
    LIBMPV_DLL = 'libmpv.2.dylib';
    {$ELSE}
    LIBMPV_DLL = 'libmpv.so.2';
    {$ENDIF}
  {$ENDIF}

type
  { ═══════════════════════════════════════════════════════════════════════════
    HANDLE TYPE
    ═══════════════════════════════════════════════════════════════════════════ }
  Pmpv_handle = ^Tmpv_handle;
  Tmpv_handle = record end;

  PPAnsiChar = ^PAnsiChar;
  PPPAnsiChar = ^PPAnsiChar;

  { ═══════════════════════════════════════════════════════════════════════════
    ERROR CODES
    ═══════════════════════════════════════════════════════════════════════════ }
const
  MPV_ERROR_SUCCESS              = 0;
  MPV_ERROR_EVENT_QUEUE_FULL     = -1;
  MPV_ERROR_NOMEM                = -2;
  MPV_ERROR_UNINITIALIZED        = -3;
  MPV_ERROR_INVALID_PARAMETER    = -4;
  MPV_ERROR_OPTION_NOT_FOUND     = -5;
  MPV_ERROR_OPTION_FORMAT        = -6;
  MPV_ERROR_OPTION_ERROR         = -7;
  MPV_ERROR_PROPERTY_NOT_FOUND   = -8;
  MPV_ERROR_PROPERTY_FORMAT      = -9;
  MPV_ERROR_PROPERTY_UNAVAILABLE = -10;
  MPV_ERROR_PROPERTY_ERROR       = -11;
  MPV_ERROR_COMMAND              = -12;
  MPV_ERROR_LOADING_FAILED       = -13;
  MPV_ERROR_AO_INIT_FAILED       = -14;
  MPV_ERROR_VO_INIT_FAILED       = -15;
  MPV_ERROR_NOTHING_TO_PLAY      = -16;
  MPV_ERROR_UNKNOWN_FORMAT       = -17;
  MPV_ERROR_UNSUPPORTED          = -18;
  MPV_ERROR_NOT_IMPLEMENTED      = -19;
  MPV_ERROR_GENERIC              = -20;

  { ═══════════════════════════════════════════════════════════════════════════
    DATA FORMATS
    ═══════════════════════════════════════════════════════════════════════════ }
  MPV_FORMAT_NONE       = 0;
  MPV_FORMAT_STRING     = 1;
  MPV_FORMAT_OSD_STRING = 2;
  MPV_FORMAT_FLAG       = 3;
  MPV_FORMAT_INT64      = 4;
  MPV_FORMAT_DOUBLE     = 5;
  MPV_FORMAT_NODE       = 6;
  MPV_FORMAT_NODE_ARRAY = 7;
  MPV_FORMAT_NODE_MAP   = 8;
  MPV_FORMAT_BYTE_ARRAY = 9;

type
  mpv_format = Integer;

  { ═══════════════════════════════════════════════════════════════════════════
    NODE STRUCTURES
    ═══════════════════════════════════════════════════════════════════════════ }
type
  Pmpv_node = ^mpv_node;
  Pmpv_node_list = ^mpv_node_list;
  Pmpv_byte_array = ^mpv_byte_array;

  mpv_byte_array = record
    data: Pointer;
    size: NativeUInt;
  end;

  mpv_node_list = record
    num: Integer;
    values: Pmpv_node;
    keys: PPAnsiChar;
  end;

  mpv_node = record
    case format: mpv_format of
      MPV_FORMAT_STRING,
      MPV_FORMAT_OSD_STRING: (str: PAnsiChar);
      MPV_FORMAT_FLAG: (flag: Integer);
      MPV_FORMAT_INT64: (int64_: Int64);
      MPV_FORMAT_DOUBLE: (double_: Double);
      MPV_FORMAT_NODE_ARRAY,
      MPV_FORMAT_NODE_MAP: (list: Pmpv_node_list);
      MPV_FORMAT_BYTE_ARRAY: (ba: Pmpv_byte_array);
      MPV_FORMAT_NONE: ();
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    EVENT TYPES
    ═══════════════════════════════════════════════════════════════════════════ }
const
  MPV_EVENT_NONE              = 0;
  MPV_EVENT_SHUTDOWN          = 1;
  MPV_EVENT_LOG_MSG           = 2;    { Renamed from LOG_MESSAGE }
  MPV_EVENT_GET_PROP_REPLY    = 3;    { Renamed from GET_PROPERTY_REPLY }
  MPV_EVENT_SET_PROP_REPLY    = 4;    { Renamed from SET_PROPERTY_REPLY }
  MPV_EVENT_COMMAND_REPLY     = 5;
  MPV_EVENT_START_FILE        = 6;
  MPV_EVENT_END_FILE          = 7;
  MPV_EVENT_FILE_LOADED       = 8;
  MPV_EVENT_IDLE              = 11;
  MPV_EVENT_TICK              = 14;
  MPV_EVENT_CLIENT_MSG        = 16;   { Renamed from CLIENT_MESSAGE }
  MPV_EVENT_VIDEO_RECONFIG    = 17;
  MPV_EVENT_AUDIO_RECONFIG    = 18;
  MPV_EVENT_SEEK              = 20;
  MPV_EVENT_PLAYBACK_RESTART  = 21;
  MPV_EVENT_PROPERTY_CHANGE   = 22;
  MPV_EVENT_QUEUE_OVERFLOW    = 24;
  MPV_EVENT_HOOK              = 25;

type
  mpv_event_id = Integer;

const
  { ═══════════════════════════════════════════════════════════════════════════
    END FILE REASONS
    ═══════════════════════════════════════════════════════════════════════════ }
  MPV_END_FILE_REASON_EOF      = 0;
  MPV_END_FILE_REASON_STOP     = 2;
  MPV_END_FILE_REASON_QUIT     = 3;
  MPV_END_FILE_REASON_ERROR    = 4;
  MPV_END_FILE_REASON_REDIRECT = 5;

  { ═══════════════════════════════════════════════════════════════════════════
    LOG LEVELS
    ═══════════════════════════════════════════════════════════════════════════ }
  MPV_LOG_LEVEL_NONE  = 0;
  MPV_LOG_LEVEL_FATAL = 10;
  MPV_LOG_LEVEL_ERROR = 20;
  MPV_LOG_LEVEL_WARN  = 30;
  MPV_LOG_LEVEL_INFO  = 40;
  MPV_LOG_LEVEL_V     = 50;
  MPV_LOG_LEVEL_DEBUG = 60;
  MPV_LOG_LEVEL_TRACE = 70;

type
  { ═══════════════════════════════════════════════════════════════════════════
    EVENT STRUCTURES
    ═══════════════════════════════════════════════════════════════════════════ }

  { Property change event data }
  Pmpv_event_property = ^Tmpv_event_property;
  Tmpv_event_property = record
    name: PAnsiChar;
    format: mpv_format;
    data: Pointer;
  end;

  { Log message event data }
  Pmpv_event_log_message = ^Tmpv_event_log_message;
  Tmpv_event_log_message = record
    prefix: PAnsiChar;
    level: PAnsiChar;
    text: PAnsiChar;
    log_level: Integer;
  end;

  { End file event data }
  Pmpv_event_end_file = ^Tmpv_event_end_file;
  Tmpv_event_end_file = record
    reason: Integer;
    error: Integer;
    playlist_entry_id: Int64;
    playlist_insert_id: Int64;
    playlist_insert_num_entries: Integer;
  end;

  { Start file event data }
  Pmpv_event_start_file = ^Tmpv_event_start_file;
  Tmpv_event_start_file = record
    playlist_entry_id: Int64;
  end;

  { Client message event data }
  Pmpv_event_client_message = ^Tmpv_event_client_message;
  Tmpv_event_client_message = record
    num_args: Integer;
    args: PPAnsiChar;
  end;

  { Hook event data }
  Pmpv_event_hook = ^Tmpv_event_hook;
  Tmpv_event_hook = record
    name: PAnsiChar;
    id: UInt64;
  end;

  { Command reply data }
  Pmpv_event_command = ^Tmpv_event_command;
  Tmpv_event_command = record
    result: mpv_node;
  end;

  { Main event structure }
  Pmpv_event = ^Tmpv_event;
  Tmpv_event = record
    event_id: mpv_event_id;
    error: Integer;
    reply_userdata: UInt64;
    data: Pointer;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    CALLBACK TYPES
    ═══════════════════════════════════════════════════════════════════════════ }
  Tmpv_wakeup_cb = procedure(d: Pointer); cdecl;
  Tmpv_render_update_fn = procedure(cb_ctx: Pointer); cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    RENDER API STRUCTURES (for custom rendering)
    ═══════════════════════════════════════════════════════════════════════════ }
const
  MPV_RENDER_PARAM_INVALID             = 0;
  MPV_RENDER_PARAM_API_TYPE            = 1;
  MPV_RENDER_PARAM_OPENGL_INIT_PARAMS  = 2;
  MPV_RENDER_PARAM_OPENGL_FBO          = 3;
  MPV_RENDER_PARAM_FLIP_Y              = 4;
  MPV_RENDER_PARAM_DEPTH               = 5;
  MPV_RENDER_PARAM_ICC_PROFILE         = 6;
  MPV_RENDER_PARAM_AMBIENT_LIGHT       = 7;
  MPV_RENDER_PARAM_X11_DISPLAY         = 8;
  MPV_RENDER_PARAM_WL_DISPLAY          = 9;
  MPV_RENDER_PARAM_ADVANCED_CONTROL    = 10;
  MPV_RENDER_PARAM_NEXT_FRAME_INFO     = 11;
  MPV_RENDER_PARAM_BLOCK_FOR_TARGET_TIME = 12;
  MPV_RENDER_PARAM_SKIP_RENDERING      = 13;
  MPV_RENDER_PARAM_DRM_DISPLAY         = 14;
  MPV_RENDER_PARAM_DRM_DRAW_SURFACE_SIZE = 15;
  MPV_RENDER_PARAM_DRM_DISPLAY_V2      = 16;

type
  Pmpv_render_context = ^Tmpv_render_context;
  Tmpv_render_context = record end;

  PPmpv_render_context = ^Pmpv_render_context;

  mpv_render_param_type = Integer;

  Pmpv_render_param = ^Tmpv_render_param;
  Tmpv_render_param = record
    type_: mpv_render_param_type;
    data: Pointer;
  end;

  Pmpv_opengl_init_params = ^Tmpv_opengl_init_params;
  Tmpv_opengl_init_params = record
    get_proc_address: function(ctx: Pointer; const name: PAnsiChar): Pointer; cdecl;
    get_proc_address_ctx: Pointer;
    extra_exts: PAnsiChar;
  end;

  Pmpv_opengl_fbo = ^Tmpv_opengl_fbo;
  Tmpv_opengl_fbo = record
    fbo: Integer;
    w: Integer;
    h: Integer;
    internal_format: Integer;
  end;

  { ═══════════════════════════════════════════════════════════════════════════
    STREAM CALLBACKS (for custom protocols)
    ═══════════════════════════════════════════════════════════════════════════ }
  Tmpv_stream_cb_read_fn = function(cookie: Pointer; buf: PAnsiChar;
    nbytes: UInt64): Int64; cdecl;
  Tmpv_stream_cb_seek_fn = function(cookie: Pointer; offset: Int64): Int64; cdecl;
  Tmpv_stream_cb_size_fn = function(cookie: Pointer): Int64; cdecl;
  Tmpv_stream_cb_close_fn = procedure(cookie: Pointer); cdecl;
  Tmpv_stream_cb_cancel_fn = procedure(cookie: Pointer); cdecl;

  Pmpv_stream_cb_info = ^Tmpv_stream_cb_info;
  Tmpv_stream_cb_info = record
    cookie: Pointer;
    read_fn: Tmpv_stream_cb_read_fn;
    seek_fn: Tmpv_stream_cb_seek_fn;
    size_fn: Tmpv_stream_cb_size_fn;
    close_fn: Tmpv_stream_cb_close_fn;
    cancel_fn: Tmpv_stream_cb_cancel_fn;
  end;

  Tmpv_stream_cb_open_ro_fn = function(user_data: Pointer; uri: PAnsiChar;
    info: Pmpv_stream_cb_info): Integer; cdecl;

var
  { ═══════════════════════════════════════════════════════════════════════════
    CORE FUNCTIONS
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_client_api_version: function(): Cardinal; cdecl;
  mpv_error_string: function(error: Integer): PAnsiChar; cdecl;
  mpv_free: procedure(data: Pointer); cdecl;
  mpv_client_name: function(ctx: Pmpv_handle): PAnsiChar; cdecl;
  mpv_client_id: function(ctx: Pmpv_handle): Int64; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    CONTEXT MANAGEMENT
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_create: function(): Pmpv_handle; cdecl;
  mpv_initialize: function(ctx: Pmpv_handle): Integer; cdecl;
  mpv_destroy: procedure(ctx: Pmpv_handle); cdecl;
  mpv_terminate_destroy: procedure(ctx: Pmpv_handle); cdecl;
  mpv_create_client: function(ctx: Pmpv_handle; const name: PAnsiChar): Pmpv_handle; cdecl;
  mpv_create_weak_client: function(ctx: Pmpv_handle; const name: PAnsiChar): Pmpv_handle; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    OPTIONS
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_set_option: function(ctx: Pmpv_handle; const name: PAnsiChar;
    format: mpv_format; data: Pointer): Integer; cdecl;
  mpv_set_option_string: function(ctx: Pmpv_handle; const name, data: PAnsiChar): Integer; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    COMMANDS
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_command: function(ctx: Pmpv_handle; args: PPAnsiChar): Integer; cdecl;
  mpv_command_node: function(ctx: Pmpv_handle; args: Pmpv_node;
    result: Pmpv_node): Integer; cdecl;
  mpv_command_string: function(ctx: Pmpv_handle; const args: PAnsiChar): Integer; cdecl;
  mpv_command_async: function(ctx: Pmpv_handle; reply_userdata: UInt64;
    args: PPAnsiChar): Integer; cdecl;
  mpv_command_node_async: function(ctx: Pmpv_handle; reply_userdata: UInt64;
    args: Pmpv_node): Integer; cdecl;
  mpv_abort_async_command: procedure(ctx: Pmpv_handle; reply_userdata: UInt64); cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    PROPERTIES
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_set_property: function(ctx: Pmpv_handle; const name: PAnsiChar;
    format: mpv_format; data: Pointer): Integer; cdecl;
  mpv_set_property_string: function(ctx: Pmpv_handle;
    const name, data: PAnsiChar): Integer; cdecl;
  mpv_set_property_async: function(ctx: Pmpv_handle; reply_userdata: UInt64;
    const name: PAnsiChar; format: mpv_format; data: Pointer): Integer; cdecl;
  mpv_get_property: function(ctx: Pmpv_handle; const name: PAnsiChar;
    format: mpv_format; data: Pointer): Integer; cdecl;
  mpv_get_property_string: function(ctx: Pmpv_handle;
    const name: PAnsiChar): PAnsiChar; cdecl;
  mpv_get_property_osd_string: function(ctx: Pmpv_handle;
    const name: PAnsiChar): PAnsiChar; cdecl;
  mpv_get_property_async: function(ctx: Pmpv_handle; reply_userdata: UInt64;
    const name: PAnsiChar; format: mpv_format): Integer; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    PROPERTY OBSERVATION
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_observe_property: function(ctx: Pmpv_handle; reply_userdata: UInt64;
    const name: PAnsiChar; format: mpv_format): Integer; cdecl;
  mpv_unobserve_property: function(ctx: Pmpv_handle;
    registered_reply_userdata: UInt64): Integer; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    EVENTS
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_wait_event: function(ctx: Pmpv_handle; timeout: Double): Pmpv_event; cdecl;
  mpv_wakeup: procedure(ctx: Pmpv_handle); cdecl;
  mpv_set_wakeup_callback: procedure(ctx: Pmpv_handle; cb: Tmpv_wakeup_cb;
    d: Pointer); cdecl;
  mpv_get_wakeup_pipe: function(ctx: Pmpv_handle): Integer; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    LOGGING
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_request_log_messages: function(ctx: Pmpv_handle;
    const min_level: PAnsiChar): Integer; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    NODE UTILITIES
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_free_node_contents: procedure(node: Pmpv_node); cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    HOOKS
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_hook_add: function(ctx: Pmpv_handle; reply_userdata: UInt64;
    const name: PAnsiChar; priority: Integer): Integer; cdecl;
  mpv_hook_continue: function(ctx: Pmpv_handle; id: UInt64): Integer; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    STREAM CALLBACKS
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_stream_cb_add_ro: function(ctx: Pmpv_handle; const protocol: PAnsiChar;
    user_data: Pointer; open_fn: Tmpv_stream_cb_open_ro_fn): Integer; cdecl;

  { ═══════════════════════════════════════════════════════════════════════════
    RENDER API FUNCTIONS
    ═══════════════════════════════════════════════════════════════════════════ }
  mpv_render_context_create: function(res: PPmpv_render_context;
    mpv: Pmpv_handle; params: Pmpv_render_param): Integer; cdecl;
  mpv_render_context_set_update_callback: procedure(ctx: Pmpv_render_context;
    callback: Tmpv_render_update_fn; callback_ctx: Pointer); cdecl;
  mpv_render_context_update: function(ctx: Pmpv_render_context): UInt64; cdecl;
  mpv_render_context_render: function(ctx: Pmpv_render_context;
    params: Pmpv_render_param): Integer; cdecl;
  mpv_render_context_report_swap: procedure(ctx: Pmpv_render_context); cdecl;
  mpv_render_context_free: procedure(ctx: Pmpv_render_context); cdecl;

{ ═══════════════════════════════════════════════════════════════════════════════
  LIBRARY LOADING FUNCTIONS
  ═══════════════════════════════════════════════════════════════════════════════ }

var
  LibMPVHandle: TLibHandle = NilHandle;
  LibMPVLoaded: Boolean = False;

function LoadLibMPV(const LibPath: string = ''): Boolean;
procedure UnloadLibMPV;
function IsLibMPVLoaded: Boolean;
function GetLibMPVVersion: string;

{ ═══════════════════════════════════════════════════════════════════════════════
  UTILITY FUNCTIONS
  ═══════════════════════════════════════════════════════════════════════════════ }
{$CALLING DEFAULT}
function MPVErrorToStr(Error: Integer): string;
function MPVEventToStr(EventID: mpv_event_id): string;
function MPVFormatToStr(AFormat: mpv_format): string;
function BuildMPVArgs(const Args: array of string): PPAnsiChar;
procedure FreeMPVArgs(Args: PPAnsiChar);
{$IFDEF WINDOWS}
  {$CALLING stdcall}
{$ELSE}
  {$CALLING cdecl}
{$ENDIF}

implementation

function LoadLibMPV(const LibPath: string): Boolean;
var
  ActualPath: string;
begin
  Result := False;

  if LibMPVLoaded then
  begin
    Result := True;
    Exit;
  end;

  if LibPath <> '' then
    ActualPath := LibPath
  else
    ActualPath := LIBMPV_DLL;

  LibMPVHandle := LoadLibrary(ActualPath);

  if LibMPVHandle = NilHandle then
    Exit;

  { Load core functions }
  Pointer(mpv_client_api_version) := GetProcAddress(LibMPVHandle, 'mpv_client_api_version');
  Pointer(mpv_error_string) := GetProcAddress(LibMPVHandle, 'mpv_error_string');
  Pointer(mpv_free) := GetProcAddress(LibMPVHandle, 'mpv_free');
  Pointer(mpv_client_name) := GetProcAddress(LibMPVHandle, 'mpv_client_name');
  Pointer(mpv_client_id) := GetProcAddress(LibMPVHandle, 'mpv_client_id');

  { Load context management }
  Pointer(mpv_create) := GetProcAddress(LibMPVHandle, 'mpv_create');
  Pointer(mpv_initialize) := GetProcAddress(LibMPVHandle, 'mpv_initialize');
  Pointer(mpv_destroy) := GetProcAddress(LibMPVHandle, 'mpv_destroy');
  Pointer(mpv_terminate_destroy) := GetProcAddress(LibMPVHandle, 'mpv_terminate_destroy');
  Pointer(mpv_create_client) := GetProcAddress(LibMPVHandle, 'mpv_create_client');
  Pointer(mpv_create_weak_client) := GetProcAddress(LibMPVHandle, 'mpv_create_weak_client');

  { Load options }
  Pointer(mpv_set_option) := GetProcAddress(LibMPVHandle, 'mpv_set_option');
  Pointer(mpv_set_option_string) := GetProcAddress(LibMPVHandle, 'mpv_set_option_string');

  { Load commands }
  Pointer(mpv_command) := GetProcAddress(LibMPVHandle, 'mpv_command');
  Pointer(mpv_command_node) := GetProcAddress(LibMPVHandle, 'mpv_command_node');
  Pointer(mpv_command_string) := GetProcAddress(LibMPVHandle, 'mpv_command_string');
  Pointer(mpv_command_async) := GetProcAddress(LibMPVHandle, 'mpv_command_async');
  Pointer(mpv_command_node_async) := GetProcAddress(LibMPVHandle, 'mpv_command_node_async');
  Pointer(mpv_abort_async_command) := GetProcAddress(LibMPVHandle, 'mpv_abort_async_command');

  { Load properties }
  Pointer(mpv_set_property) := GetProcAddress(LibMPVHandle, 'mpv_set_property');
  Pointer(mpv_set_property_string) := GetProcAddress(LibMPVHandle, 'mpv_set_property_string');
  Pointer(mpv_set_property_async) := GetProcAddress(LibMPVHandle, 'mpv_set_property_async');
  Pointer(mpv_get_property) := GetProcAddress(LibMPVHandle, 'mpv_get_property');
  Pointer(mpv_get_property_string) := GetProcAddress(LibMPVHandle, 'mpv_get_property_string');
  Pointer(mpv_get_property_osd_string) := GetProcAddress(LibMPVHandle, 'mpv_get_property_osd_string');
  Pointer(mpv_get_property_async) := GetProcAddress(LibMPVHandle, 'mpv_get_property_async');

  { Load property observation }
  Pointer(mpv_observe_property) := GetProcAddress(LibMPVHandle, 'mpv_observe_property');
  Pointer(mpv_unobserve_property) := GetProcAddress(LibMPVHandle, 'mpv_unobserve_property');

  { Load events }
  Pointer(mpv_wait_event) := GetProcAddress(LibMPVHandle, 'mpv_wait_event');
  Pointer(mpv_wakeup) := GetProcAddress(LibMPVHandle, 'mpv_wakeup');
  Pointer(mpv_set_wakeup_callback) := GetProcAddress(LibMPVHandle, 'mpv_set_wakeup_callback');
  Pointer(mpv_get_wakeup_pipe) := GetProcAddress(LibMPVHandle, 'mpv_get_wakeup_pipe');

  { Load logging }
  Pointer(mpv_request_log_messages) := GetProcAddress(LibMPVHandle, 'mpv_request_log_messages');

  { Load node utilities }
  Pointer(mpv_free_node_contents) := GetProcAddress(LibMPVHandle, 'mpv_free_node_contents');

  { Load hooks }
  Pointer(mpv_hook_add) := GetProcAddress(LibMPVHandle, 'mpv_hook_add');
  Pointer(mpv_hook_continue) := GetProcAddress(LibMPVHandle, 'mpv_hook_continue');

  { Load stream callbacks (optional) }
  Pointer(mpv_stream_cb_add_ro) := GetProcAddress(LibMPVHandle, 'mpv_stream_cb_add_ro');

  { Load render API (optional) }
  Pointer(mpv_render_context_create) := GetProcAddress(LibMPVHandle, 'mpv_render_context_create');
  Pointer(mpv_render_context_set_update_callback) := GetProcAddress(LibMPVHandle, 'mpv_render_context_set_update_callback');
  Pointer(mpv_render_context_update) := GetProcAddress(LibMPVHandle, 'mpv_render_context_update');
  Pointer(mpv_render_context_render) := GetProcAddress(LibMPVHandle, 'mpv_render_context_render');
  Pointer(mpv_render_context_report_swap) := GetProcAddress(LibMPVHandle, 'mpv_render_context_report_swap');
  Pointer(mpv_render_context_free) := GetProcAddress(LibMPVHandle, 'mpv_render_context_free');

  { Check essential functions }
  if not Assigned(mpv_create) or
     not Assigned(mpv_initialize) or
     not Assigned(mpv_terminate_destroy) or
     not Assigned(mpv_command) or
     not Assigned(mpv_set_property) or
     not Assigned(mpv_get_property) or
     not Assigned(mpv_wait_event) then
  begin
    UnloadLibMPV;
    Exit;
  end;

  LibMPVLoaded := True;
  Result := True;
end;

procedure UnloadLibMPV;
begin
  if LibMPVHandle <> NilHandle then
  begin
    FreeLibrary(LibMPVHandle);
    LibMPVHandle := NilHandle;
  end;

  mpv_client_api_version := nil;
  mpv_error_string := nil;
  mpv_free := nil;
  mpv_client_name := nil;
  mpv_client_id := nil;
  mpv_create := nil;
  mpv_initialize := nil;
  mpv_destroy := nil;
  mpv_terminate_destroy := nil;
  mpv_create_client := nil;
  mpv_create_weak_client := nil;
  mpv_set_option := nil;
  mpv_set_option_string := nil;
  mpv_command := nil;
  mpv_command_node := nil;
  mpv_command_string := nil;
  mpv_command_async := nil;
  mpv_command_node_async := nil;
  mpv_abort_async_command := nil;
  mpv_set_property := nil;
  mpv_set_property_string := nil;
  mpv_set_property_async := nil;
  mpv_get_property := nil;
  mpv_get_property_string := nil;
  mpv_get_property_osd_string := nil;
  mpv_get_property_async := nil;
  mpv_observe_property := nil;
  mpv_unobserve_property := nil;
  mpv_wait_event := nil;
  mpv_wakeup := nil;
  mpv_set_wakeup_callback := nil;
  mpv_get_wakeup_pipe := nil;
  mpv_request_log_messages := nil;
  mpv_free_node_contents := nil;
  mpv_hook_add := nil;
  mpv_hook_continue := nil;
  mpv_stream_cb_add_ro := nil;
  mpv_render_context_create := nil;
  mpv_render_context_set_update_callback := nil;
  mpv_render_context_update := nil;
  mpv_render_context_render := nil;
  mpv_render_context_report_swap := nil;
  mpv_render_context_free := nil;

  LibMPVLoaded := False;
end;

function IsLibMPVLoaded: Boolean;
begin
  Result := LibMPVLoaded;
end;

function GetLibMPVVersion: string;
var
  Version: Cardinal;
begin
  if not LibMPVLoaded or not Assigned(mpv_client_api_version) then
  begin
    Result := 'Not loaded';
    Exit;
  end;

  Version := mpv_client_api_version();
  Result := Format('%d.%d', [Version shr 16, Version and $FFFF]);
end;

{$CALLING DEFAULT}
function MPVErrorToStr(Error: Integer): string;
var
  ErrStr: PAnsiChar;
begin
  if LibMPVLoaded and Assigned(mpv_error_string) then
  begin
    ErrStr := mpv_error_string(Error);
    if ErrStr <> nil then
      Result := string(ErrStr)
    else
      Result := Format('Unknown error (%d)', [Error]);
  end
  else
  begin
    case Error of
      MPV_ERROR_SUCCESS:              Result := 'Success';
      MPV_ERROR_EVENT_QUEUE_FULL:     Result := 'Event queue full';
      MPV_ERROR_NOMEM:                Result := 'Out of memory';
      MPV_ERROR_UNINITIALIZED:        Result := 'Not initialized';
      MPV_ERROR_INVALID_PARAMETER:    Result := 'Invalid parameter';
      MPV_ERROR_OPTION_NOT_FOUND:     Result := 'Option not found';
      MPV_ERROR_OPTION_FORMAT:        Result := 'Option format error';
      MPV_ERROR_OPTION_ERROR:         Result := 'Option error';
      MPV_ERROR_PROPERTY_NOT_FOUND:   Result := 'Property not found';
      MPV_ERROR_PROPERTY_FORMAT:      Result := 'Property format error';
      MPV_ERROR_PROPERTY_UNAVAILABLE: Result := 'Property unavailable';
      MPV_ERROR_PROPERTY_ERROR:       Result := 'Property error';
      MPV_ERROR_COMMAND:              Result := 'Command error';
      MPV_ERROR_LOADING_FAILED:       Result := 'Loading failed';
      MPV_ERROR_AO_INIT_FAILED:       Result := 'Audio output init failed';
      MPV_ERROR_VO_INIT_FAILED:       Result := 'Video output init failed';
      MPV_ERROR_NOTHING_TO_PLAY:      Result := 'Nothing to play';
      MPV_ERROR_UNKNOWN_FORMAT:       Result := 'Unknown format';
      MPV_ERROR_UNSUPPORTED:          Result := 'Unsupported';
      MPV_ERROR_NOT_IMPLEMENTED:      Result := 'Not implemented';
      MPV_ERROR_GENERIC:              Result := 'Generic error';
    else
      Result := Format('Unknown error (%d)', [Error]);
    end;
  end;
end;

function MPVEventToStr(EventID: mpv_event_id): string;
begin
  case EventID of
    MPV_EVENT_NONE:              Result := 'None';
    MPV_EVENT_SHUTDOWN:          Result := 'Shutdown';
    MPV_EVENT_LOG_MSG:           Result := 'Log message';
    MPV_EVENT_GET_PROP_REPLY:    Result := 'Get property reply';
    MPV_EVENT_SET_PROP_REPLY:    Result := 'Set property reply';
    MPV_EVENT_COMMAND_REPLY:     Result := 'Command reply';
    MPV_EVENT_START_FILE:        Result := 'Start file';
    MPV_EVENT_END_FILE:          Result := 'End file';
    MPV_EVENT_FILE_LOADED:       Result := 'File loaded';
    MPV_EVENT_IDLE:              Result := 'Idle';
    MPV_EVENT_TICK:              Result := 'Tick';
    MPV_EVENT_CLIENT_MSG:        Result := 'Client message';
    MPV_EVENT_VIDEO_RECONFIG:    Result := 'Video reconfig';
    MPV_EVENT_AUDIO_RECONFIG:    Result := 'Audio reconfig';
    MPV_EVENT_SEEK:              Result := 'Seek';
    MPV_EVENT_PLAYBACK_RESTART:  Result := 'Playback restart';
    MPV_EVENT_PROPERTY_CHANGE:   Result := 'Property change';
    MPV_EVENT_QUEUE_OVERFLOW:    Result := 'Queue overflow';
    MPV_EVENT_HOOK:              Result := 'Hook';
  else
    Result := Format('Unknown event (%d)', [EventID]);
  end;
end;

function MPVFormatToStr(AFormat: mpv_format): string;
begin
  case AFormat of
    MPV_FORMAT_NONE:       Result := 'None';
    MPV_FORMAT_STRING:     Result := 'String';
    MPV_FORMAT_OSD_STRING: Result := 'OSD string';
    MPV_FORMAT_FLAG:       Result := 'Flag';
    MPV_FORMAT_INT64:      Result := 'Int64';
    MPV_FORMAT_DOUBLE:     Result := 'Double';
    MPV_FORMAT_NODE:       Result := 'Node';
    MPV_FORMAT_NODE_ARRAY: Result := 'Node array';
    MPV_FORMAT_NODE_MAP:   Result := 'Node map';
    MPV_FORMAT_BYTE_ARRAY: Result := 'Byte array';
  else
    Result := Format('Unknown format (%d)', [AFormat]);
  end;
end;

function BuildMPVArgs(const Args: array of string): PPAnsiChar;
type
  TPAnsiCharArray = array[0..MaxInt div SizeOf(PAnsiChar) - 1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;
var
  I, Count: Integer;
  ArgArray: PPAnsiCharArray;
begin
  Count := High(Args) - Low(Args) + 1;
  GetMem(ArgArray, (Count + 1) * SizeOf(PAnsiChar));

  for I := 0 to Count - 1 do
  begin
    ArgArray^[I] := StrNew(PAnsiChar(AnsiString(Args[Low(Args) + I])));
  end;
  ArgArray^[Count] := nil;

  Result := PPAnsiChar(ArgArray);
end;

procedure FreeMPVArgs(Args: PPAnsiChar);
type
  TPAnsiCharArray = array[0..MaxInt div SizeOf(PAnsiChar) - 1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;
var
  I: Integer;
  ArgArray: PPAnsiCharArray;
begin
  if Args = nil then
    Exit;

  ArgArray := PPAnsiCharArray(Args);
  I := 0;
  while ArgArray^[I] <> nil do
  begin
    StrDispose(ArgArray^[I]);
    Inc(I);
  end;

  FreeMem(Args);
end;

initialization
  LibMPVHandle := NilHandle;
  LibMPVLoaded := False;

finalization
  UnloadLibMPV;

end.
