unit BasicZip;

{ Basic classes for reading ZIP files.
  Only supports standard ZIP file that are compressed using methods
  0 (no compression) and 8 (Deflate, using ZLib). Does not support multi-disk
  or encrypted ZIP files or ZIP files with a comment at the end. }

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  TZipCompressionMethod = (cmNone, cmDeflate);

  TZipExtractAction = (eaExtract, eaSkip, eaAbort);

  TZipEntry = class;

  TZipExtractEvent = procedure(Sender: TObject; const Entry: TZipEntry;
    var Action: TZipExtractAction) of Object;

  TZipReader = class
  {$REGION 'Internal Declarations'}
  strict private
    FStream: TStream;
    FOwnsStream: Boolean;
    FEntries: TObjectList<TZipEntry>;
    FOnExtract: TZipExtractEvent;
  strict private
    procedure ReadDirectory;
  {$ENDREGION 'Internal Declarations'}
  public
    { Opens a ZIP file by filename }
    constructor Create(const ZipFilename: String); overload;

    { Opens a ZIP file in a stream.
      NOTE: The ZIP reader does NOT become owner of the stream. }
    constructor Create(const ZipStream: TStream); overload;

    destructor Destroy; override;

    { Extracts the entire ZIP file to the given target directory.
      NOTE: If you want to extract individual entries, then you can use the
      TZipEntry.Extract methods.
      To be notified of extraction progress, assign the FOnExtract event. }
    procedure Extract(const TargetDir: String);

    { The entries in the ZIP file }
    property Entries: TObjectList<TZipEntry> read FEntries;

    { Subscribe to this event to be notified when entries in the ZIP file are
      extracted (using the Extract method). The paremeters are:
      Entry: The ZIP entry that is about to be extracted.
      Action: The action that should be taken. You can change this value in
        the event:
        -eaExtract: proceed extraction (default value)
        -eaSkip: skip this ZIP entry
        -eaAbort: abort extraction of this and any other ZIP entries. }
    property OnExtract: TZipExtractEvent read FOnExtract write FOnExtract;
  end;

  { Single entry in a ZIP file }
  TZipEntry = class
  {$REGION 'Internal Declarations'}
  private
    FSourceStream: TStream;
    FFilename: String;
    FCompressedSize: Integer;
    FUncompressedSize: Integer;
    FAttributes: Integer;
    FRelativeOffset: Integer;
    FCompressionMethod: TZipCompressionMethod;
    FLastMod: TDateTime;
  private
    procedure ExtractStored(const TargetStream: TStream);
    procedure ExtractDeflate(const TargetStream: TStream);
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const SourceStream: TStream);

    { Extracts the entry to a stream. }
    procedure Extract(const TargetStream: TStream);

    { Filename, including optional (relative) directory names.
      Will always include backslashes (\) for directory delimiters. }
    property Filename: String read FFilename;

    { Size of entry in compressed form }
    property CompressedSize: Integer read FCompressedSize;

    { Size of entry in uncompressed form }
    property UncompressedSize: Integer read FUncompressedSize;

    { File attribytes (see faXXX constants) }
    property Attributes: Integer read FAttributes;

    { The compression method used for this entry }
    property CompressionMethod: TZipCompressionMethod read FCompressionMethod;

    { Time and date of last modification }
    property LastMod: TDateTime read FLastMod;
  end;

  { Exception class for exceptions raised by TZipReader }
  EZipError = class(Exception)
  public
    constructor Create(const Msg: String);
  end;

resourcestring
  RS_ZIP_ERROR = 'Error in ZIP file';
  RS_INVALID_EOCD = 'Invalid End of Central Directory in ZIP file';
  RS_NO_MULTI_DISK = 'Multipe-disk ZIP files are not supported';
  RS_INVALID_CFH = 'Invalid Central File header in ZIP file';
  RS_INVALID_LFH = 'Invalid Local File header';
  RS_UNSUPPORTED_ZIP = 'Unsupported compression method in ZIP file';

implementation

uses
  ZLib;

{ EZipError }

constructor EZipError.Create(const Msg: String);
begin
  inherited Create(RS_ZIP_ERROR + ': ' + Msg);
end;

function ZCompressCheck(const Code: Integer): Integer;
begin
  Result := Code;

  if (Code < 0) then
    raise EZCompressionError.Create(string(_z_errmsg[2 - Code]));
end;

{ TZipReader }

type
  TZipEndOfCentralDirectory = packed record
    Signature: Longword;
    DiskNumber: Smallint;
    DirectoryDiskNumber: Smallint;
    EntryCountOnDisk: Smallint;
    EntryCount: Smallint;
    Size: Longint;
    Offset: Longint;
    CommentLength: Smallint;
  end;

type
  TZipCentralFileHeader = packed record
    Signature: Longword;
    VersionMadeBy: Word;
    VersionNeeded: Word;
    GeneralPurposeFlags: Word;
    CompressionMethod: Word;
    LastModFileDateTime: Longint;
    CRC32: Longword;
    CompressedSize: Longint;
    UncompressedSize: Longint;
    FilenameLength: Smallint;
    ExtraFieldLength: Smallint;
    FileCommentLength: Smallint;
    DiskNumberStart: Smallint;
    InternalFileAttributes: Word;
    ExternalFileAttributes: Longword;
    RelativeOffset: Longint;
  end;

type
  TZipLocalFileHeader = packed record
    Signature: Longword;
    VersionNeeded: Word;
    GeneralPurposeFlags: Word;
    CompressionMethod: Word;
    LastModFileDateTime: Longint;
    CRC32: Longword;
    CompressedSize: Longint;
    UncompressedSize: Longint;
    FilenameLength: Smallint;
    ExtraFieldLength: Smallint;
  end;

constructor TZipReader.Create(const ZipFilename: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(ZipFilename, fmOpenRead or fmShareDenyWrite);
  Create(Stream);
  FOwnsStream := True;
end;

constructor TZipReader.Create(const ZipStream: TStream);
begin
  inherited Create;
  FStream := ZipStream;
  FEntries := TObjectList<TZipEntry>.Create;
  ReadDirectory;
end;

destructor TZipReader.Destroy;
begin
  FEntries.Free;
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

procedure TZipReader.Extract(const TargetDir: String);
var
  Dir, Filename: String;
  Entry: TZipEntry;
  Action: TZipExtractAction;
  Stream: TFileStream;
begin
  Dir := IncludeTrailingPathDelimiter(TargetDir);
  for Entry in FEntries do
  begin
    Action := eaExtract;
    if Assigned(FOnExtract) then
      FOnExtract(Self, Entry, Action);
    if (Action = eaAbort) then
      Break;
    if (Action = eaExtract) then
    begin
      Filename := Dir + Entry.FFilename;
      ForceDirectories(ExtractFilePath(Filename));
      Stream := TFileStream.Create(Filename, fmCreate);
      try
        Entry.Extract(Stream);
      finally
        Stream.Free;
      end;
      FileSetDate(Filename, DateTimeToFileDate(Entry.LastMod));
    end;
  end;
end;

procedure TZipReader.ReadDirectory;
var
  I: Integer;
  EndOfCentralDirectory: TZipEndOfCentralDirectory;
  FileHeader: TZipCentralFileHeader;
  Entry: TZipEntry;
  Filename, ExtraField, FileComment: AnsiString;
begin
  FillChar(EndOfCentralDirectory, SizeOf(EndOfCentralDirectory), 0);
  if (FStream.Size > SizeOf(TZipEndOfCentralDirectory)) then
  begin
    FStream.Seek(-SizeOf(EndOfCentralDirectory), soFromEnd);
    FStream.ReadBuffer(EndOfCentralDirectory, SizeOf(EndOfCentralDirectory));
  end;

  if (EndOfCentralDirectory.Signature <> $06054B50) then
    raise EZipError.Create(RS_INVALID_EOCD);
  if (EndOfCentralDirectory.DiskNumber <> 0) or (EndOfCentralDirectory.DirectoryDiskNumber <> 0) then
    raise EZipError.Create(RS_NO_MULTI_DISK);

  FStream.Position := EndOfCentralDirectory.Offset;
  for I := 0 to EndOfCentralDirectory.EntryCount - 1 do
  begin
    FStream.ReadBuffer(FileHeader, SizeOf(FileHeader));
    if (FileHeader.Signature <> $02014B50) then
      raise EZipError.Create(RS_INVALID_CFH);

    SetLength(Filename, FileHeader.FilenameLength);
    if (FileHeader.FilenameLength > 0) then
      FStream.ReadBuffer(Filename[1], FileHeader.FilenameLength);

    SetLength(ExtraField, FileHeader.ExtraFieldLength);
    if (FileHeader.ExtraFieldLength > 0) then
      FStream.ReadBuffer(ExtraField[1], FileHeader.ExtraFieldLength);

    SetLength(FileComment, FileHeader.FileCommentLength);
    if (FileHeader.FileCommentLength > 0) then
      FStream.ReadBuffer(FileComment[1], FileHeader.FileCommentLength);

    { Skip Directory entries }
    if ((FileHeader.ExternalFileAttributes and faDirectory) = 0) then
    begin
      if (not (FileHeader.CompressionMethod in [0, 8])) then
        raise EZipError.Create(RS_UNSUPPORTED_ZIP);

      Entry := TZipEntry.Create(FStream);
      FEntries.Add(Entry);

      {$WARNINGS OFF}
      Entry.FFilename := StringReplace(Filename, '/', '\', [rfReplaceAll]);
      {$WARNINGS ON}

      Entry.FCompressedSize := FileHeader.CompressedSize;
      Entry.FUncompressedSize := FileHeader.UncompressedSize;
      Entry.FAttributes := FileHeader.ExternalFileAttributes;
      Entry.FRelativeOffset := FileHeader.RelativeOffset;
      if (FileHeader.CompressionMethod = 8) then
        Entry.FCompressionMethod := cmDeflate
      else
        Entry.FCompressionMethod := cmNone;
      Entry.FLastMod := FileDateToDateTime(FileHeader.LastModFileDateTime);
    end;
  end;
end;

{ TZipEntry }

constructor TZipEntry.Create(const SourceStream: TStream);
begin
  inherited Create;
  FSourceStream := SourceStream;
end;

procedure TZipEntry.Extract(const TargetStream: TStream);
var
  Header: TZipLocalFileHeader;
begin
  FSourceStream.Position := FRelativeOffset;
  FSourceStream.ReadBuffer(Header, SizeOf(Header));

  if (Header.Signature <> $04034B50) then
    raise EZipError.Create(RS_INVALID_LFH);
  if (not (Header.CompressionMethod in [0, 8])) then
    raise EZipError.Create(RS_UNSUPPORTED_ZIP);
  FCompressedSize := Header.CompressedSize;
  FUncompressedSize := Header.UncompressedSize;
  FLastMod := FileDateToDateTime(Header.LastModFileDateTime);
  FSourceStream.Seek(Header.FilenameLength + Header.ExtraFieldLength, soFromCurrent);

  case Header.CompressionMethod of
    0: ExtractStored(TargetStream);
    8: ExtractDeflate(TargetStream);
  else
    Assert(False);
  end;
end;

procedure TZipEntry.ExtractDeflate(const TargetStream: TStream);
const
  BUFFER_SIZE = 32768;
var
  ZStream: TZStreamRec;
  ZResult: Integer;
  InBuffer: array [0..BUFFER_SIZE - 1] of AnsiChar;
  OutBuffer: array [0..BUFFER_SIZE - 1] of AnsiChar;
  InSize, BytesLeft: Integer;
  OutSize: Integer;
begin
  FillChar(ZStream, SizeOf(TZStreamRec), 0);

  ZCompressCheck(inflateInit2_(ZStream, -15, ZLIB_VERSION, SizeOf(ZStream)));

  BytesLeft := FCompressedSize;
  if (BytesLeft > BUFFER_SIZE) then
    InSize := FSourceStream.Read(InBuffer, BUFFER_SIZE)
  else
    InSize := FSourceStream.Read(InBuffer, BytesLeft);

  while (InSize > 0) do
  begin
    Dec(BytesLeft, InSize);
    ZStream.next_in := @InBuffer[0];
    ZStream.avail_in := InSize;

    repeat
      ZStream.next_out := @OutBuffer[0];
      ZStream.avail_out := BUFFER_SIZE;

      ZCompressCheck(inflate(ZStream, Z_NO_FLUSH));

      OutSize := BUFFER_SIZE - ZStream.avail_out;

      TargetStream.Write(OutBuffer, OutSize);
    until (ZStream.avail_in = 0) and (ZStream.avail_out > 0);

    if (BytesLeft > BUFFER_SIZE) then
      InSize := FSourceStream.Read(InBuffer, BUFFER_SIZE)
    else
      InSize := FSourceStream.Read(InBuffer, BytesLeft);
  end;

  repeat
    ZStream.next_out := @OutBuffer[0];
    ZStream.avail_out := BUFFER_SIZE;

    ZResult := ZCompressCheck(inflate(ZStream, Z_FINISH));

    OutSize := BUFFER_SIZE - ZStream.avail_out;

    TargetStream.Write(OutBuffer, OutSize);
  until (ZResult = Z_STREAM_END) and (ZStream.avail_out > 0);

  ZCompressCheck(inflateEnd(ZStream));
end;

procedure TZipEntry.ExtractStored(const TargetStream: TStream);
begin
  TargetStream.CopyFrom(FSourceStream, FUncompressedSize);
end;

end.
