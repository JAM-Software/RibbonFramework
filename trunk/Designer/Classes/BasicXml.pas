unit BasicXml;

{ Basic classes for simple reading and writing of XML files. These classes are
  not fully compliant with the XML specification, but are blazing fast.
  Only a subset of the XML specification is supported:
  -Only UTF-8 encoded XML streams are supported.
  -Element and Attribute names may NOT contain Unicode (they are treated as
   AnsiStrings).
  -Element and Attribute values may contain Unicode however. These are converted
   to UTF-8.
  -There is no support for CDATA sections (<![CDATA[...]]>), entities
   (<!ENTITY...>), processing instructions (<?...?>), comments (<!--...-->),
   doctypes (<!DOCTYPE...>) and notations (<!NOTATION...>). These kinds of
   data cannot be written, and they are ignored when read.
  -There is no support for entity or parameter references (internal or external)
   except for predefined character references (like &amp;).
  -There is no support for namespaces, but you can use element and attribute
   names containing colons. }

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  RawByteChar = AnsiChar;
  PRawByteChar = PAnsiChar;

type
  { Class for building RawByteStrings using multiple append-operations. Mimics
    the .NET StringBuilder class somewhat, but works with 8-bit Ansi strings.
    Use this class when concatenating a lot of strings instead of concatenating
    these strings manually. This class is more effecient because it uses
    buffering to prevent frequent memory reallocations and copying of string
    values. }
  TRawByteStringBuilder = class
  {$REGION 'Internal Declarations'}
  strict private
    FBuffer: PRawByteChar;
    FCurrent: PRawByteChar;
    FCapacity: Integer;
    FInitialCapacity: Integer;
    FDelta: Integer;
    FLength: Integer;
    function GetChar(const Index: Integer): RawByteChar;
    procedure SetChar(const Index: Integer; const Value: RawByteChar);
  strict private
    procedure Grow(const Size: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates an instance using an initial capacity and delta. Capacity is the
      initial amount of memory reserved for the string. If the string needs to
      grow beyond this, the capacity is increased with steps of Delta bytes. }
    constructor Create(const Capacity: Integer = 256;
      const Delta: Integer = 256);
    destructor Destroy; override;

    { Appends a string or a range of bytes to the end of the buffer.
      NOTE: avoid constructions like Append(Str1 + Str2) and split these up
      into two separate Append calls since that is more efficient. }
    procedure Append(const Value: RawByteString); overload;
    procedure Append(const Value: RawByteString; const Index, Length: Integer); overload;
    procedure Append(const Buffer: Pointer; const Size: Integer); overload;

    { As Append, but adds additional CR+LF }
    procedure AppendLn;

    { Returns the string value of the buffer.
      NOTE: ToString makes a new string and copies the buffer to that string.
      If you don't need to make any modifications to the returned string, you
      can also access the buffer directly using the Buffer and Length
      properties. This is more efficient since it doesn't involve a string
      allocation and copy. }
    function ToRawByteString: RawByteString;
    function ToString: RawByteString; reintroduce;

    { Starts with a new empty string and releases all memory associated with
      the current string. Use Reset instead to keep this memory. }
    procedure Clear;

    { Starts with a new empty string without releasing the currently allocated
      memory. This is more efficient then Clear when this string builder is
      used to create many different strings. }
    procedure Reset;

    { Current capacity (number of reserved bytes) }
    property Capacity: Integer read FCapacity;

    { Current length of the string }
    property Length: Integer read FLength;

    { Individual characters in the string, where Index starts at 0 }
    property Chars[const Index: Integer]: RawByteChar read GetChar write SetChar; default;

    { The currently allocated buffer. If you only need to access the string
      value in the buffer, using this property is more efficient than the
      ToString method (see ToString).
      NOTE: This value may change as the buffer grows }
    property Buffer: PRawByteChar read FBuffer;
  end;

type
  { Represents a writer that provides a fast, non-cached, forward-only means of
    generating XML data.

    Example: to write out the following XML...

      <?xml version="1.0" encoding="UTF-8"?>
      <MyRoot>
        <Foo Bar="123">Some Content</Foo>
        <Baz/>
      </MyRoot>

    ...you would do something like this:

      var
        Writer: TXmlWriter;
        Xml: RawByteString;
      begin
        Writer := TXmlWriter.Create;
        try
          Writer.Indent := True;
          Writer.WriteStartElement('MyRoot');
          Writer.WriteStartElement('Foo');
          Writer.WriteAttribute('Bar', 123);
          Writer.WriteContent('Some Content');
          Writer.WriteEndElement; // </Foo>
          Writer.WriteStartElement('Baz');
          Writer.WriteEndElement; // <Baz/>
          Writer.WriteEndElement; // </MyRoot>
          Xml := Writer.AsXml; // Returns the XML
        finally
          Writer.Free;
        end;
      end;

    Note that you can set the Indent property to True to create formatted
    (indented) XML. This is useful for human reading, but adds extra whitespace.
    The Indent property is False by default to reduce size.

    Since the writer is forward-only and sequential, you MUST write any
    attributes before you write element content, and you MUST write element
    content before you write any subelements.

    Using a XmlWriter may be a bit more work than using an XML DOM, but it is
    very fast and memory efficient. }
  TXmlWriter = class
  {$REGION 'Internal Declarations'}
  strict private
    FIndent: Boolean;
    FIndentCount: Integer;
    FIndentSize: Integer;
    FIndentString: RawByteString;
    FOutput: TRawByteStringBuilder;
    FWriteState: (wsStart, wsElement, wsContent, wsEndElement);
    FElementStack: array of RawByteString;
    FElementStackTop: Integer;
    FHasSubNodes: array of Boolean;
    FHasNodes: Boolean;
  strict private
    procedure Flush;
    procedure WriteXmlEncoded(const Value: String);
    procedure WriteContentStart;
  private
    procedure WriteStartElementRaw(const ElementName: RawByteString);
  {$ENDREGION}
  public
    { Converts a Boolean to string with value 'true' or 'false'. }
    class function XmlEncodeBoolean(const Value: Boolean): String; static;
  public
    constructor Create;
    destructor Destroy; override;

    { Resets the writer and starts with a new document.
      The document will automatically have a <?xml> declaration that declares
      the UTF-8 encoding. }
    procedure Reset;

    { Start a new XML element <ElementName>. ElementName may contain a
      namespace-prefix. Must be closed later with WriteEndElement.
      NOTE: ElementName may NOT contain Unicode. }
    procedure WriteStartElement(const ElementName: String); inline;

    { Closes the element written with the last WriteStartElement call. }
    procedure WriteEndElement;

    { Writes an attribute for the current element. The attribute name may
      contain a namespace-prefix. You can only call this method after you have
      created an element using WriteStartElement.
      NOTE: Name may NOT contain Unicode.
      NOTE: Value may contain Unicode. It will be converted to UTF-8 and XML
      encoded when needed.
      NOTE: When Value is an empty string, no attribute will be written. }
    procedure WriteAttribute(const Name, Value: String); overload;

    { Write attribute value as an Integer }
    procedure WriteAttribute(const Name: String; const Value: Integer); overload;

    { Write attribute value as a boolean. The value will be stored as a string
      with value 'true' or 'false' }
    procedure WriteAttribute(const Name: String; const Value: Boolean); overload;

    { Writes content for the current element. The content goes between the
      start of the element (<Element>, see WriteStartElement) and the end of
      the element(</Element>, see WriteEndElement).
      NOTE: Value may contain Unicode. It will be converted to UTF-8 and XML
      encoded when needed. }
    procedure WriteContent(const Value: String); overload;

    { Write content as an Integer }
    procedure WriteContent(const Value: Integer); overload;

    { Write content as a boolean. The content will be stored as a string
      with value 'true' or 'false' }
    procedure WriteContent(const Value: Boolean); overload;

    { Returns the contents as a RawByteString. }
    function AsXml: RawByteString;
    function ToString: RawByteString; reintroduce;

    { Whether to indent elements. Default False.
      Setting this to True is useful for human reading, but adds extra
      whitespace. The Indent property is False by default to reduce size. }
    property Indent: Boolean read FIndent write FIndent default False;

    { Number of spaces to use when indenting. This setting is used when the
      Indent property is set to true. Defaults to 2. }
    property IndentCount: Integer read FIndentCount write FIndentCount default 2;
  end;

type
  TXmlNodeType = (ntNone, ntElement, ntEndElement, ntAttribute, ntContent);

type
  { Represents a reader that provides fast, non-cached, forward-only access to
    XML data.

    For example, you can use the reader like this:

      var
        Reader: TXmlReader;
        Xml: RawByteString;
      begin
        Xml := LoadXmlFromSomewhere;
        Reader := TXmlReader.Create;
        try
          Reader.SetXml(Xml);
          while Reader.Read do
          begin
            case Reader.NodeType of
              ntElement:
                WriteLn('Element name: ' + Reader.Name);
              ntEndElement:
                WriteLn('End element');
              ntAttribute:
                begin
                  WriteLn('Attribute name: ' + Reader.Name);
                  WriteLn('Attribute value: ' + Reader.Value);
                end;
              ntContent:
                WriteLn('Element content: ' + Reader.Value);
            end;
          end;
        finally
          Reader.Free;
        end;
      end;

    Since the reader is forward-only and sequential, you can only handle
    elements and attributes as you encounter them in the stream. You cannot
    search for specific elements or attributes. However, you can use the
    TXmlDocument class as a light-weight DOM-like layer on top of an XML
    stream. }
  TXmlReader = class
  {$REGION 'Internal Declarations'}
  private
    FXml: RawByteString;
    FCur: PRawByteChar;
    FNodeType: TXmlNodeType;
    FReadState: (rsStart, rsElement, rsContent, rsEndElement);
    FName: PRawByteChar;
    FValue: PRawByteChar;
    FValueSeparator: array [0..1] of RawByteChar;
  private
    function GetName: String;
    function GetValue: String;
    function GetValueAsInteger: Integer;
    function GetValueAsBoolean: Boolean;
  private
    function SkipWhiteSpace(const P: PRawByteChar): PRawByteChar;
    function SkipName(const P: PRawByteChar): PRawByteChar;
    function SkipText(const P, Terminator: PRawByteChar;
      const IgnoreCase: Boolean): PRawByteChar;
    class function StringEqual(P, Target: PRawByteChar;
      const IgnoreCase: Boolean): Boolean; static;
    procedure ParseXmlDeclaration;
    procedure SkipComment;
    procedure SkipCData;
    procedure SkipDTD;
    procedure SkipProcessingInstruction;
    function AttributeValue(const Name: PRawByteChar): String;
    function LocateAttributeValue(const Name: PRawByteChar): Boolean;
  {$ENDREGION}
  public
    { XML decodes a string (replaces &...; entities with their values) }
    class function XmlDecode(const S: RawByteString): RawByteString; static;

    { Converts a string to a boolean. Returns True if the string has the value
      '1', 'True', 'Yes', 'T' or 'Y' (case-insensitive), or False otherwise. }
    class function XmlDecodeBoolean(const S: String): Boolean; static;
  public
    { Sets the XML to parse. Raises an exception if the start of the XML data
      is not valid, or not encoded using UTF-8.
      Xml may start with a UTF-8 BOM marker, but that is not needed. Other
      BOM markers will raise an exception. }
    procedure SetXml(const Xml: RawByteString);

    { Reads the next node. Returns True if successfull, or False if the end of
      the XML has been reached.
      When the function returns True, you can use NodeType, Name and Value to
      inspect the current node. Raises exceptions when the XML is invalid.
      Unsupported constructs (like comments, CDATA, entities etc.) are ignored
      (without raising exceptions) }
    function Read: Boolean;

    { Returns the type of the current node, after a call to Read.
        ntNone: when Read has not been called.
        ntElement: element (for example, <item>).
        ntEndElement: element end tag (for example, </item>).
        ntAttribute: attribute (for example, id="123")
        ntContent: the (text) content of an element }
    property NodeType: TXmlNodeType read FNodeType;

    { Returns the name of the current node, depending on the node type:
        ntNone: empty string
        ntElement: tag name (may include a namespace-prefix)
        ntEndElement: tag name if specified (as in </item>) or empty string
          for a short-cut end element (as in <item/>)
        ntAttribute: attribute name (may include a namespace-prefix)
        ntContent: empty string }
    property Name: String read GetName;

    { Returns the value of the current node, depending on the node type:
        ntNone: empty string
        ntElement: empty string
        ntEndElement: empty string
        ntAttribute: attribute value
        ntContent: (text) content
      The value will be UTF-8 decoded and Xml decoded (that is, entity
      references like &qout; will be replaced with their values).
      Leading and trailing whitespace is preserved in the value. }
    property Value: String read GetValue;

    { Returns Value as an integer, or 0 when the value is empty or no valid
      integer. }
    property ValueAsInteger: Integer read GetValueAsInteger;

    { Returns Value as a boolean. Returns True when the string value equals
      'True', 'Yes', 'T', 'Y' or '1' (case insensitive), or False otherwise. }
    property ValueAsBoolean: Boolean read GetValueAsBoolean;
  end;

type
  { Represents a single Read-Only element in a TXmlDocument.
    An element has a name, parent, attributes, children and content, which
    can all be accessed through the properties of the TXmlElement class. }
  TXmlElement = class
  {$REGION 'Internal Declarations'}
  private
    type
      TEnumerator = class
      private
        FIndex: Integer;
        FElement: TXmlElement;
      public
        constructor Create(const Element: TXmlElement);
        function GetCurrent: TXmlElement;
        function MoveNext: Boolean;
        property Current: TXmlElement read GetCurrent;
      end;
  private
    FName: PRawByteChar;
    FReader: TXmlReader;
    FParent: TXmlElement;
    FAttributes: array of PRawByteChar;
    FChildren: array of TXmlElement;
    FChildCount: Integer;
    FContent: PRawByteChar;
    function GetChild(const Index: Integer): TXmlElement;
    function GetAttributeCount: Integer;
    function GetAttributeName(const Index: Integer): String;
    function GetAttributeValue(const Index: Integer): String;
    function GetAttributeAsString(const Name: String): String;
    function GetAttributeAsInteger(const Name: String): Integer;
    function GetAttributeAsBoolean(const Name: String): Boolean;
    function GetContentAsString: String;
    function GetContentAsInteger: Integer;
    function GetContentAsBoolean: Boolean;
  private
    constructor Create(const Reader: TXmlReader; const Parent: TXmlElement;
      const Name: PRawByteChar); overload;
    procedure AddChild(const Child: TXmlElement);
    procedure SetAttributes(const Attributes: array of PRawByteChar;
      const Count: Integer);
    function GetName: String;
    function FindAttribute(const Name: String): PRawByteChar;
  {$ENDREGION}
  public
    constructor Create; overload;
    destructor Destroy; override;

    { Support for the for..in construct
      (for example: for Child in Element do...) }
    function GetEnumerator: TEnumerator;

    { Wheter the element has an attribute with the given name (case-sensitive). }
    function HasAttribute(const Name: String): Boolean;

    { Retrieves line number of the (start of this) element.
      NOTE: This method searches the entire XML for this element, so it can
      take some time for really big XML documents. So, you should only call
      this when really necessary, for example when showing an error message. }
    function GetLineNumber: Integer;

    { Value of an attribute as a String, indexed by attribute name.
      Returns Def if the attribute is not found. }
    function AttributeAsStringDef(const Name: String; const Def: String): String;

    { Value of an attribute as an Integer, indexed by attribute name.
      Returns Def if the attribute is not found. }
    function AttributeAsIntegerDef(const Name: String; const Def: Integer): Integer;

    { Value of an attribute as a Boolean, indexed by attribute name.
      Returns Def if the attribute is not found. }
    function AttributeAsBooleanDef(const Name: String; const Def: Boolean): Boolean;

    { Parent element. Equals nil for the root element of the document. }
    property Parent: TXmlElement read FParent;

    { Name of the element. May include a namespace-prefix. }
    property Name: String read GetName;

    { Number of children. }
    property ChildCount: Integer read FChildCount;

    { Children.
      Note: you can also use the for..in construct to enumerate children
      (for example: for Child in Element do...) }
    property Children[const Index: Integer]: TXmlElement read GetChild; default;

    { Number of attributes }
    property AttributeCount: Integer read GetAttributeCount;

    { Names of the attributes, indexed by attribute number.
      The name may include a namespace-prefix. }
    property AttributeNames[const Index: Integer]: String read GetAttributeName;

    { Values of the attributes, indexed by attribute number. }
    property AttributeValues[const Index: Integer]: String read GetAttributeValue;

    { Value of an attribute as a String, indexed by attribute name.
      Returns an empty string if the attribute is not found. }
    property AttributeAsString[const Name: String]: String read GetAttributeAsString;

    { Value of an attribute as an Integer, indexed by attribute name.
      Returns 0 if the attribute is not found or is not a valid Integer. }
    property AttributeAsInteger[const Name: String]: Integer read GetAttributeAsInteger;

    { Value of an attribute as a Boolean, indexed by attribute name.
      Returns True when the string value equals 'True', 'Yes' 'T', 'Y' or '1'
      (case insensitive), or False otherwise. }
    property AttributeAsBoolean[const Name: String]: Boolean read GetAttributeAsBoolean;

    { Content of the element as a String. }
    property ContentAsString: String read GetContentAsString;

    { Content of the element as an Integer.
      Returns 0 if the element does not have content, or the content is not a
      valid Integer. }
    property ContentAsInteger: Integer read GetContentAsInteger;

    { Content of the element as a Boolean.
      Returns True when the string value equals 'True', 'Yes', 'T', 'Y' or '1'
      (case insensitive), or False otherwise. }
    property ContentAsBoolean: Boolean read GetContentAsBoolean;
  end;

  { Very lightweight Read-Only XML DOM-like document that's also suitable for
    larger documents. Speed and memory efficiency is achieved by the following:
    -The document only contains Element nodes. This in contrast to the W3C DOM
     model, which also uses seperate nodes for attributes and the (text) content
     of elements.
    -The Element nodes do not contain any content themselves, but only pointers
     to the underlying XML string.
    -There is no support for adding, deleting and changing nodes.

    Example:

      var
        Doc: TXmlDocument;
        Xml: RawByteString;
        Element: TXmlElement;
      begin
        Xml := LoadXmlFromSomewhere;
        Doc := TXmlDocument.Create;
        try
          Doc.LoadFromXml(Xml);
          for Element in Doc.Root do
          begin
            WriteLn('Element: ' + Element.Name);
            WriteLn('Attribute 0 name: ' + Element.AttributeNames[0]);
            WriteLn('Attribute 1 value: ' + Element.AttributeValues[1]);
            WriteLn('Atrribute "Foo": ' + Element.AttributeAsString['Foo']);
            WriteLn('Content: ' + Element.ContentAsString);
          end;
        finally
          Doc.Free;
        end;
      end; }
  TXmlDocument = class
  {$REGION 'Internal Declarations'}
  strict private
    FReader: TXmlReader;
    FRoot: TXmlElement;
  {$ENDREGION}
  public
    constructor Create;
    destructor Destroy; override;

    { Clears the XML document }
    procedure Clear;

    { Loads the document from an XML string }
    procedure LoadFromXml(const Xml: RawByteString);

    { Loads the document from a stream }
    procedure LoadFromStream(const Stream: TStream);

    { Loads the document from a file }
    procedure LoadFromFile(const Filename: String);

    { Root element (or nil of no valid XML document has been loaded) }
    property Root: TXmlElement read FRoot;
  end;

resourcestring
  RS_CHAR_INDEX_OUT_OF_RANGE = 'Character index out of range';
  RS_EMPTY_ATTRIBUTE_NAME = 'XML Attribute name may not be empty';
  RS_INVALID_ATTRIBUTE_POSITION = 'XML Attribute can only be added to an Element';
  RS_INVALID_CONTENT_POSITION = 'XML Content can only be added to an Element';
  RS_ELEMENT_MISMATCH = 'XML End Element does not match XML Start Element';
  RS_EMPTY_ELEMENT_NAME = 'XML Element name may not be empty';
  RS_MULTIPLE_ROOT_NODES = 'Cannot add more than 1 root node';
  RS_NESTED_XML_DECL = 'Invalid XML document: nested XML declarations are not supported';
  RS_INVALID_LT = 'Invalid XML document: "<" not allowed within element';
  RS_GT_EXPECTED = 'Invalid XML end element: ">" expected';
  RS_EQ_EXPECTED = 'Invalid XML attribute: "=" expected';
  RS_VALUE_EXPECTED = 'Invalid XML attribute: value expected';
  RS_QUOTE_EXPECTED = 'Invalid XML attribute: quote expected';
  RS_INVALID_XML_DATA = 'Invalid XML data';
  RS_EMPTY_XML = 'Cannot parse empty XML';
  RS_UNSUPPORTED_ENCODING = 'Unsupported XML encoding';
  RS_INVALID_XML_DECL = 'Invalid XML declaration';
  RS_INVALID_CDATA = 'Invalid XML CDATA';
  RS_INVALID_COMMENT = 'Invalid XML comment';
  RS_INVALID_DTD = 'Invalid XML DTD';
  RS_INVALID_NAME = 'Invalid XML name';
  RS_INVALID_PI = 'Invalid XML processing instruction';
  RS_READ_MULTIPLE_ROOT_NODES = 'Invalid XML document: document cannot have multiple root nodes';
  RS_NO_END_ELEMENT = 'Invalid XML document: end element without start element';

implementation

uses
  AnsiStrings,
  StrUtils,
  RtlConsts;

const
  S_WHITESPACE = [#9..#13, #32];
  S_NAME_START = ['a'..'z', 'A'..'Z', '_'];
  S_NAME = S_NAME_START + ['0'..'9', '-', '.', ':'];
  S_FALSE = 'false';
  S_TRUE = 'true';

{ TRawByteStringBuilder }

procedure TRawByteStringBuilder.Append(const Value: RawByteString);
begin
  Append(PRawByteChar(Value), System.Length(Value));
end;

procedure TRawByteStringBuilder.Append(const Buffer: Pointer; const Size: Integer);
begin
  if (Size > 0) then
  begin
    if ((FLength + Size) > FCapacity) then
      Grow(Size);
    Move(Buffer^, FCurrent^, Size);
    Inc(FCurrent, Size);
    Inc(FLength, Size);
  end;
end;

procedure TRawByteStringBuilder.Append(const Value: RawByteString;
  const Index, Length: Integer);
begin
  if (Length > 0) then
  begin
    Assert(Index <= System.Length(Value));
    Assert(Index + Length - 1 <= System.Length(Value));
    Append(@Value[Index], Length);
  end;
end;

procedure TRawByteStringBuilder.AppendLn;
begin
  Append(sLineBreak);
end;

procedure TRawByteStringBuilder.Clear;
begin
  Reset;
  FCapacity := FInitialCapacity;
  ReallocMem(FBuffer, FCapacity);
  FCurrent := FBuffer;
end;

constructor TRawByteStringBuilder.Create(const Capacity, Delta: Integer);
begin
  inherited Create;
  if (Capacity < 0) then
    FCapacity := 0
  else
    FCapacity := Capacity;
  if (Delta < 1) then
    FDelta := 1
  else
    FDelta := Delta;
  FInitialCapacity := FCapacity;
  GetMem(FBuffer, FCapacity);
  FCurrent := FBuffer;
end;

destructor TRawByteStringBuilder.Destroy;
begin
  FreeMem(FBuffer);
  inherited;
end;

function TRawByteStringBuilder.GetChar(const Index: Integer): RawByteChar;
begin
  if (Index < 0) or (Index >= FLength) then
    raise ERangeError.Create(RS_CHAR_INDEX_OUT_OF_RANGE);
  Result := FBuffer[Index];
end;

procedure TRawByteStringBuilder.Grow(const Size: Integer);
var
  BytesNeeded: Integer;
begin
  BytesNeeded := FLength + Size - FCapacity;
  BytesNeeded := ((BytesNeeded + FDelta - 1) div FDelta) * FDelta;
  Inc(FCapacity, BytesNeeded);
  ReallocMem(FBuffer, FCapacity);
  FCurrent := FBuffer;
  Inc(FCurrent, FLength);
end;

procedure TRawByteStringBuilder.Reset;
begin
  FLength := 0;
  FCurrent := FBuffer;
end;

procedure TRawByteStringBuilder.SetChar(const Index: Integer;
  const Value: RawByteChar);
begin
  if (Index < 0) or (Index >= FLength) then
    raise ERangeError.Create(RS_CHAR_INDEX_OUT_OF_RANGE);
  FBuffer[Index] := Value;
end;

function TRawByteStringBuilder.ToRawByteString: RawByteString;
begin
  if (FBuffer = nil) then
    Result := ''
  else
    SetString(Result, FBuffer, FLength);
end;

function TRawByteStringBuilder.ToString: RawByteString;
begin
  Result := ToRawByteString;
end;

{ TXmlWriter }

function TXmlWriter.AsXml: RawByteString;
begin
  Flush;
  Result := FOutput.ToString;
end;

constructor TXmlWriter.Create;
begin
  inherited Create;
  FOutput := TRawByteStringBuilder.Create;
  FIndentCount := 2;
  Reset;
end;

destructor TXmlWriter.Destroy;
begin
  FOutput.Free;
  inherited;
end;

procedure TXmlWriter.Flush;
begin
  { Close any elements that are still open. }
  while (FElementStackTop > 0) do
    WriteEndElement;
end;

procedure TXmlWriter.Reset;
begin
  FOutput.Clear;
  FOutput.Append('<?xml version="1.0" encoding="UTF-8"?>');
  FIndentSize := 0;
  FWriteState := wsStart;
  FElementStackTop := 0;
  FHasNodes := False;
end;

function TXmlWriter.ToString: RawByteString;
begin
  Flush;
  Result := FOutput.ToString;
end;

procedure TXmlWriter.WriteAttribute(const Name, Value: String);
begin
  if (Value = '') then
    Exit;
  if (Name = '') then
    raise EInvalidOperation.Create(RS_EMPTY_ATTRIBUTE_NAME);
  if (FWriteState <> wsElement) then
    raise EInvalidOperation.Create(RS_INVALID_ATTRIBUTE_POSITION);

  FOutput.Append(' ');
  {$WARNINGS OFF}
  FOutput.Append(RawByteString(Name));
  {$WARNINGS ON}
  FOutput.Append('="');
  WriteXmlEncoded(Value);
  FOutput.Append('"');
end;

procedure TXmlWriter.WriteAttribute(const Name: String;
  const Value: Integer);
begin
  WriteAttribute(Name, IntToStr(Value));
end;

procedure TXmlWriter.WriteAttribute(const Name: String;
  const Value: Boolean);
begin
  WriteAttribute(Name, XmlEncodeBoolean(Value));
end;

procedure TXmlWriter.WriteContent(const Value: Integer);
begin
  WriteContent(IntToStr(Value));
end;

procedure TXmlWriter.WriteContent(const Value: Boolean);
begin
  WriteContent(XmlEncodeBoolean(Value));
end;

procedure TXmlWriter.WriteContent(const Value: String);
begin
  if (Value = '') then
    Exit;
  WriteContentStart;
  WriteXmlEncoded(Value);
end;

procedure TXmlWriter.WriteContentStart;
begin
  if (FWriteState in [wsStart, wsEndElement]) then
    raise EInvalidOperation.Create(RS_INVALID_CONTENT_POSITION);
  if (FWriteState = wsElement) then
  begin
    FOutput.Append('>');
    FWriteState := wsContent;
  end;
end;

procedure TXmlWriter.WriteEndElement;
begin
  if (FElementStackTop <= 0) then
    raise EInvalidOperation.Create(RS_ELEMENT_MISMATCH);

  Dec(FElementStackTop);
  if (FHasSubNodes[FElementStackTop]) then
  begin
    if (FWriteState = wsElement) then
      FOutput.Append('>');
    FOutput.AppendLn;
    if (FIndent) then
    begin
      if (FIndentSize >= FIndentCount) then
        Dec(FIndentSize, FIndentCount);
      FOutput.Append(FIndentString, 1, FIndentSize);
    end;
    FOutput.Append('</');
    FOutput.Append(FElementStack[FElementStackTop]);
    FOutput.Append('>');
  end
  else
  begin
    if (FWriteState = wsElement) then
      FOutput.Append('/>')
    else
    begin
      FOutput.Append('</');
      FOutput.Append(FElementStack[FElementStackTop]);
      FOutput.Append('>');
    end;
    if (FIndent) and (FIndentSize >= FIndentCount) then
      Dec(FIndentSize, FIndentCount);
  end;

  FWriteState := wsEndElement;
end;

procedure TXmlWriter.WriteStartElement(const ElementName: String);
var
  Name: RawByteString;
begin
  {$WARNINGS OFF}
  Name := RawByteString(ElementName);
  {$WARNINGS ON}
  WriteStartElementRaw(Name);
end;

procedure TXmlWriter.WriteStartElementRaw(const ElementName: RawByteString);
begin
  if (ElementName = '') then
    raise EInvalidOperation.Create(RS_EMPTY_ELEMENT_NAME);
  if (FHasNodes) and (FElementStackTop = 0) then
    raise EInvalidOperation.Create(RS_MULTIPLE_ROOT_NODES);

  if (FWriteState = wsElement) then
  begin
    FOutput.Append('>');
    FWriteState := wsEndElement;
  end;

  FOutput.AppendLn;
  if (FIndent) then
  begin
    FOutput.Append(FIndentString, 1, FIndentSize);
    Inc(FIndentSize, FIndentCount);
    if (FIndentSize > Length(FIndentString)) then
    begin
      SetLength(FIndentString, FIndentSize);
      FillChar(FIndentString[1], FIndentSize, ' ');
    end;
  end;

  FOutput.Append('<');
  FOutput.Append(ElementName);
  FWriteState := wsElement;

  if (FElementStackTop >= Length(FElementStack)) then
  begin
    SetLength(FElementStack, FElementStackTop + 8);
    SetLength(FHasSubNodes, FElementStackTop + 8);
  end;
  if (FElementStackTop > 0) then
    FHasSubNodes[FElementStackTop - 1] := True;
  FElementStack[FElementStackTop] := ElementName;
  FHasSubNodes[FElementStackTop] := False;
  Inc(FElementStackTop);
  FHasNodes := True;
end;

procedure TXmlWriter.WriteXmlEncoded(const Value: String);
var
  S: RawByteString;
  C: RawByteChar;
  I: Integer;
begin
  S := UTF8Encode(Value);
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      #1..#31:
        begin
          FOutput.Append('&#x');
          {$WARNINGS OFF}
          FOutput.Append(RawByteString(IntToHex(Ord(C), 2)));
          {$WARNINGS ON}
          FOutput.Append(';');
        end;
      '&' : FOutput.Append('&amp;');
      '''': FOutput.Append('&apos;');
      '"' : FOutput.Append('&quot;');
      '<' : FOutput.Append('&lt;');
      '>' : FOutput.Append('&gt;');
    else
      FOutput.Append(C);
    end;
  end;
end;

class function TXmlWriter.XmlEncodeBoolean(const Value: Boolean): String;
begin
  if (Value) then
    Result := S_TRUE
  else
    Result := S_FALSE;
end;

{ TXmlReader }

function TXmlReader.AttributeValue(const Name: PRawByteChar): String;
begin
  if LocateAttributeValue(Name) then
    Result := GetValue
  else
    Result := '';
end;

function TXmlReader.GetName: String;
var
  P: PRawByteChar;
  I: Integer;
begin
  if (FName = nil) then
    Result := ''
  else
  begin
    P := SkipName(FName);
    if (P = nil) or (P <= FName) then
      Result := ''
    else
    begin
      { This is *MUCH* faster than typecasting to a String! }
      SetLength(Result, P - FName);
      P := FName;
      for I := 1 to Length(Result) do
      begin
        Result[I] := WideChar(P^);
        Inc(P);
      end;
    end;
  end;
end;

function TXmlReader.GetValue: String;
var
  P, Start: PRawByteChar;
  S: RawByteString;
  I: Integer;
begin
  if (FValue = nil) then
    Result := ''
  else
  begin
    if (FValueSeparator[0] = '<') then
      Start := FValue
    else
      Start := FValue + 1;
    P := SkipText(Start, FValueSeparator, False);
    if (P = nil) or (P <= Start) then
      Result := ''
    else
    begin
      { This is *MUCH* faster than doing a Copy }
      I := P - Start;
      SetLength(S, I);
      Move(Start^, S[1], I);
      S := XmlDecode(S);
      Result := UTF8ToString(S);
    end;
  end;
end;

function TXmlReader.GetValueAsBoolean: Boolean;
begin
  Result := XmlDecodeBoolean(GetValue);
end;

function TXmlReader.GetValueAsInteger: Integer;
begin
  Result := StrToIntDef(GetValue, 0);
end;

function TXmlReader.LocateAttributeValue(const Name: PRawByteChar): Boolean;
var
  P: PRawByteChar;
begin
  if Assigned(Name) then
  begin
    P := SkipName(Name);
    P := SkipWhiteSpace(P);
    Assert(Assigned(P) and (P^ = '='));
    Inc(P);
    P := SkipWhiteSpace(P);
    Assert(Assigned(P) and (P^ in ['''', '"']));
    FValueSeparator[0] := P^;
    FValue := P;
    Result := True;
  end
  else
  begin
    FValue := nil;
    Result := False;
  end;
end;

procedure TXmlReader.ParseXmlDeclaration;
var
  P, Q: PRawByteChar;
  C: RawByteChar;
begin
  P := AnsiStrPos(FCur, '?>');
  if (P = nil) then
    raise EInvalidOperation.Create(RS_INVALID_XML_DECL);
  C := P^;
  P^ := #0;
  try
    Q := TextPos(FCur, 'encoding');
    if (Q = nil) then
      Exit;
    Inc(Q, 8);
    Q := SkipWhiteSpace(Q);
    if (Q^ <> '=') then
      Exit;
    Inc(Q);
    Q := SkipWhiteSpace(Q);
    if (Q^ in ['''', '"']) then
      Inc(Q);
    Q := SkipWhiteSpace(Q);
    if (not StringEqual(Q, 'UTF-8', True)) then
      raise EInvalidOperation.Create(RS_UNSUPPORTED_ENCODING);
  finally
    P^ := C;
    FCur := P;
    Inc(FCur, 2);
  end;
end;

function TXmlReader.Read: Boolean;
var
  CurWithWhitespace: PRawByteChar;
  C: RawByteChar;
begin
  FName := nil;
  FValue := nil;
  Result := False;
  while True do
  begin
    CurWithWhitespace := FCur;
    FCur := SkipWhiteSpace(FCur);
    Result := Assigned(FCur);
    if (not Result) then
      Exit;

    if (StringEqual(FCur, '<?xml', True)) then
      raise EInvalidOperation.Create(RS_NESTED_XML_DECL);

    if (StringEqual(FCur, '<![CDATA[', False))  then
      SkipCData
    else if (StringEqual(FCur, '<!--', False)) then
      SkipComment
    else if (StringEqual(FCur, '<!', False)) then
      SkipDTD
    else if (StringEqual(FCur, '<?', False)) then
      SkipProcessingInstruction
    else
    begin
      if (FCur^ = '<') then
      begin
        if (FReadState = rsElement) then
          raise EInvalidOperation.Create(RS_INVALID_LT);
        if ((FCur + 1)^ = '/') then
        begin
          FNodeType := ntEndElement;
          Inc(FCur, 2);
          FName := FCur;
          FCur := SkipName(FCur);
          FCur := SkipWhiteSpace(FCur);
          if (FCur^ <> '>') then
            raise EInvalidOperation.Create(RS_GT_EXPECTED);
          Inc(FCur);
          FReadState := rsEndElement;
        end
        else
        begin
          FNodeType := ntElement;
          Inc(FCur);
          FName := FCur;
          FCur := SkipName(FCur);
          FReadState := rsElement;
        end;
        Break;
      end
      else if (FCur^ = '>') and (FReadState = rsElement) then
      begin
        Inc(FCur);
        FReadState := rsContent;
      end
      else if (FCur^ = '/') and (FReadState = rsElement) then
      begin
        Inc(FCur);
        if (FCur^ <> '>') then
          raise EInvalidOperation.Create(RS_GT_EXPECTED);
        Inc(FCur);
        FReadState := rsEndElement;
        FNodeType := ntEndElement;
        Break;
      end
      else
      begin
        if (FReadState = rsElement) then
        begin
          FNodeType := ntAttribute;
          FName := FCur;
          FCur := SkipName(FCur);
          FCur := SkipWhiteSpace(FCur);
          if (FCur = nil) or (FCur^ <> '=') then
            raise EInvalidOperation.Create(RS_EQ_EXPECTED);
          Inc(FCur);
          FCur := SkipWhiteSpace(FCur);
          if (FCur = nil) then
            raise EInvalidOperation.Create(RS_VALUE_EXPECTED);
          C := FCur^;
          if (not (C in ['''', '"'])) then
            raise EInvalidOperation.Create(RS_QUOTE_EXPECTED);
          FValue := FCur;
          Inc(FCur);
          FValueSeparator[0] := C;
          FCur := SkipText(FCur, FValueSeparator, False);
          Inc(FCur);
          Break;
        end
        else if (FReadState = rsContent) then
        begin
          FNodeType := ntContent;
          FValue := CurWithWhitespace;
          FValueSeparator[0] := '<';
          FCur := SkipText(FCur, FValueSeparator, False);
          Break;
        end
        else
          raise EInvalidOperation.Create(RS_INVALID_XML_DATA);
      end;
    end;
  end;
end;

procedure TXmlReader.SetXml(const Xml: RawByteString);
var
  BOM: TBytes;
  BOMSize: Integer;
  Encoding: TEncoding;
begin
  if (Xml = '') then
    raise EInvalidOperation.Create(RS_EMPTY_XML);

  BOMSize := 0;
  if (Length(Xml) >= 3) then
  begin
    SetLength(BOM, 3);
    Move(Xml[1], BOM[0], 3);
    Encoding := nil;
    BOMSize := TEncoding.GetBufferEncoding(BOM, Encoding);

    if (Encoding <> TEncoding.Default) and (Encoding <> TEncoding.UTF8) then
      raise EInvalidOperation.Create(RS_UNSUPPORTED_ENCODING);
  end;
  FXml := Xml;
  FCur := @FXml[1];
  Inc(FCur, BOMSize);
  if StringEqual(FCur, '<?xml', True) then
    ParseXmlDeclaration;
  FNodeType := ntNone;
  FReadState := rsStart;
  FName := '';
  FValue := '';
end;

procedure TXmlReader.SkipCData;
begin
  FCur := AnsiStrPos(FCur, ']]>');
  if (FCur = nil) then
    raise EInvalidOperation.Create(RS_INVALID_CDATA);
  Inc(FCur, 3);
end;

procedure TXmlReader.SkipComment;
begin
  FCur := AnsiStrPos(FCur, '-->');
  if (FCur = nil) then
    raise EInvalidOperation.Create(RS_INVALID_COMMENT);
  Inc(FCur, 3);
end;

procedure TXmlReader.SkipDTD;
begin
  FCur := AnsiStrPos(FCur, '>');
  if (FCur = nil) then
    raise EInvalidOperation.Create(RS_INVALID_DTD);
  Inc(FCur);
end;

function TXmlReader.SkipName(const P: PRawByteChar): PRawByteChar;
begin
  Assert(Assigned(P));
  if (not (P^ in S_NAME_START)) then
    raise EInvalidOperation.Create(RS_INVALID_NAME);
  Result := P;
  Inc(Result);
  while (Result^ <> #0) and (Result^ in S_NAME) do
    Inc(Result);
end;

procedure TXmlReader.SkipProcessingInstruction;
begin
  FCur := AnsiStrPos(FCur, '?>');
  if (FCur = nil) then
    raise EInvalidOperation.Create(RS_INVALID_PI);
  Inc(FCur, 2);
end;

function TXmlReader.SkipText(const P, Terminator: PRawByteChar;
  const IgnoreCase: Boolean): PRawByteChar;
begin
  Assert(Assigned(P));
  Assert(Assigned(Terminator));
  Result := P;
  while (Result^ <> #0) and (not StringEqual(Result, Terminator, IgnoreCase)) do
    Inc(Result);
end;

function TXmlReader.SkipWhiteSpace(const P: PRawByteChar): PRawByteChar;
begin
  if (P = nil) or (P^ = #0) then
    Exit(nil);
  Result := P;
  while (Result^ <> #0) and (Result^ in S_WHITESPACE) do
    Inc(Result);
  if (Result^ = #0) then
    Result := nil;
end;

class function TXmlReader.StringEqual(P, Target: PRawByteChar;
  const IgnoreCase: Boolean): Boolean;
begin
  Assert(Assigned(P));
  Assert(Assigned(Target));
  Result := False;
  if (P = nil) or (P^ = #0) then
    Exit;

  if (IgnoreCase) then
  begin
    while (P^ <> #0) and (Target^ <> #0) and (UpCase(P^) = UpCase(Target^)) do
    begin
      Inc(P);
      Inc(Target);
    end;
    if (Target^ = #0) then
      Exit(True);
  end
  else
  begin
    while (P^ <> #0) and (Target^ <> #0) and (P^ = Target^) do
    begin
      Inc(P);
      Inc(Target);
    end;
    if (Target^ = #0) then
      Exit(True);
  end;
end;

class function TXmlReader.XmlDecode(const S: RawByteString): RawByteString;
var
  I, J, C: Integer;
  Entity: RawByteString;
begin
  Result := S;
  I := 1;
  while True do
  begin
    C := 0;
    I := AnsiStrings.PosEx('&', Result, I);
    if (I = 0) then
      Break;
    J := AnsiStrings.PosEx(';', Result, I + 1);
    if (J = 0) then
      Break;
    Entity := Copy(Result, I + 1, J - I - 1);
    if (Entity = 'amp') then
      Entity := '&'
    else if (Entity = 'apos') then
      Entity := ''''
    else if (Entity = 'quot') then
      Entity := '"'
    else if (Entity = 'lt') then
      Entity := '<'
    else if (Entity = 'gt') then
      Entity := '>'
    else if (Copy(Entity, 1, 2) = '#x') then
    begin
      C := StrToIntDef('$' + Copy(String(Entity), 3, MaxInt), 0);
      Entity := '';
    end
    else if (Entity <> '') and (Entity[1] = '#') then
    begin
      C := StrToIntDef('$' + Copy(String(Entity), 2, MaxInt), 0);
      Entity := '';
    end
    else
      Entity := '';

    if (C > 0) then
    begin
      if (C < 256) then
        Entity := AnsiChar(C)
      else
        Entity := UTF8Encode(Char(C));
    end;

    if (Entity <> '') then
      Result := Copy(Result, 1, I - 1) + Entity + Copy(Result, J + 1, MaxInt);

    Inc(I);
  end;
end;

class function TXmlReader.XmlDecodeBoolean(const S: String): Boolean;
begin
  Result := (S = '1') or SameText(S, 'True') or SameText(S, 'Yes')
    or SameText(S, 'T') or SameText(S, 'Y');
end;

{ TXmlElement.TEnumerator }

constructor TXmlElement.TEnumerator.Create(const Element: TXmlElement);
begin
  FElement := Element;
  FIndex := -1;
end;

function TXmlElement.TEnumerator.GetCurrent: TXmlElement;
begin
  Result := FElement.FChildren[FIndex];
end;

function TXmlElement.TEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < FElement.FChildCount - 1);
  if Result then
    Inc(FIndex);
end;

{ TXmlElement }

procedure TXmlElement.AddChild(const Child: TXmlElement);
begin
  if (FChildCount >= Length(FChildren)) then
    SetLength(FChildren, FChildCount + 16);
  FChildren[FChildCount] := Child;
  Inc(FChildCount);
end;

constructor TXmlElement.Create(const Reader: TXmlReader;
  const Parent: TXmlElement; const Name: PRawByteChar);
begin
  inherited Create;
  Assert(Assigned(Reader));
  Assert(Assigned(Name));
  FReader := Reader;
  FParent := Parent;
  FName := Name;
  if Assigned(FParent) then
    FParent.AddChild(Self);
end;

function TXmlElement.AttributeAsBooleanDef(const Name: String;
  const Def: Boolean): Boolean;
begin
  if FReader.LocateAttributeValue(FindAttribute(Name)) then
    Result := FReader.ValueAsBoolean
  else
    Result := Def;
end;

function TXmlElement.AttributeAsIntegerDef(const Name: String;
  const Def: Integer): Integer;
begin
  if FReader.LocateAttributeValue(FindAttribute(Name)) then
    Result := FReader.ValueAsInteger
  else
    Result := Def;
end;

function TXmlElement.AttributeAsStringDef(const Name, Def: String): String;
begin
  if FReader.LocateAttributeValue(FindAttribute(Name)) then
    Result := FReader.Value
  else
    Result := Def;
end;

constructor TXmlElement.Create;
begin
  raise EInvalidOperation.Create('Do not create an XML element directly');
end;

destructor TXmlElement.Destroy;
var
  I: Integer;
begin
  for I := 0 to FChildCount - 1 do
    FChildren[I].Free;
  FChildren := nil;
  inherited;
end;

function TXmlElement.FindAttribute(const Name: String): PRawByteChar;
var
  I: Integer;
  S: RawByteString;
  PName: PRawByteChar;
begin
  if (Name = '') then
    Exit('');
  {$WARNINGS OFF}
  S := RawByteString(Name);
  {$WARNINGS ON}
  PName := @S[1];
  I := Length(S);
  for Result in FAttributes do
    if (not (Result[I] in S_NAME)) and FReader.StringEqual(Result, PName, False) then
      Exit;
  Result := nil;
end;

function TXmlElement.GetAttributeAsBoolean(const Name: String): Boolean;
begin
  if FReader.LocateAttributeValue(FindAttribute(Name)) then
    Result := FReader.ValueAsBoolean
  else
    Result := False;
end;

function TXmlElement.GetAttributeAsInteger(const Name: String): Integer;
begin
  if FReader.LocateAttributeValue(FindAttribute(Name)) then
    Result := FReader.ValueAsInteger
  else
    Result := 0;
end;

function TXmlElement.GetAttributeAsString(const Name: String): String;
begin
  Result := FReader.AttributeValue(FindAttribute(Name));
end;

function TXmlElement.GetAttributeCount: Integer;
begin
  Result := Length(FAttributes);
end;

function TXmlElement.GetAttributeName(const Index: Integer): String;
begin
  if (Index < 0) or (Index >= Length(FAttributes)) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  FReader.FName := FAttributes[Index];
  Result := FReader.Name;
end;

function TXmlElement.GetAttributeValue(const Index: Integer): String;
begin
  if (Index < 0) or (Index >= Length(FAttributes)) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FReader.AttributeValue(FAttributes[Index]);
end;

function TXmlElement.GetChild(const Index: Integer): TXmlElement;
begin
  if (Index < 0) or (Index >= FChildCount) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FChildren[Index];
end;

function TXmlElement.GetContentAsBoolean: Boolean;
begin
  FReader.FValueSeparator[0] := '<';
  FReader.FValue := FContent;
  Result := FReader.ValueAsBoolean;
end;

function TXmlElement.GetContentAsInteger: Integer;
begin
  FReader.FValueSeparator[0] := '<';
  FReader.FValue := FContent;
  Result := FReader.ValueAsInteger;
end;

function TXmlElement.GetContentAsString: String;
begin
  FReader.FValueSeparator[0] := '<';
  FReader.FValue := FContent;
  Result := FReader.Value;
end;

function TXmlElement.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TXmlElement.GetLineNumber: Integer;
var
  P: PRawByteChar;
begin
  P := @FReader.FXml[1];
  Result := 1;
  while (P < FName) and (P^ <> #0) do
  begin
    if (P^ = #10) then
      Inc(Result);
    Inc(P);
  end;
end;

function TXmlElement.GetName: String;
begin
  FReader.FName := FName;
  Result := FReader.Name;
end;

function TXmlElement.HasAttribute(const Name: String): Boolean;
begin
  Result := (FindAttribute(Name) <> nil);
end;

procedure TXmlElement.SetAttributes(const Attributes: array of PRawByteChar;
  const Count: Integer);
begin
  SetLength(FAttributes, Count);
  if (Count > 0) then
    Move(Attributes[0], FAttributes[0], Count * SizeOf(PRawByteChar));
end;

{ TXmlDocument }

procedure TXmlDocument.Clear;
begin
  FreeAndNil(FRoot);
end;

constructor TXmlDocument.Create;
begin
  inherited Create;
  FReader := TXmlReader.Create;
end;

destructor TXmlDocument.Destroy;
begin
  Clear;
  FReader.Free;
  inherited;
end;

procedure TXmlDocument.LoadFromFile(const Filename: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXmlDocument.LoadFromStream(const Stream: TStream);
var
  Size: Integer;
  Xml: RawByteString;
begin
  Size := Stream.Size;
  if (Size > 0) then
  begin
    SetLength(Xml, Size);
    Stream.Position := 0;
    Stream.ReadBuffer(Xml[1], Size);
    LoadFromXml(Xml);
  end;
end;

procedure TXmlDocument.LoadFromXml(const Xml: RawByteString);
var
  CurrentElement: TXmlElement;
  Attributes: array of PRawByteChar;
  AttributeCount: Integer;
begin
  Clear;
  FReader.SetXml(Xml);
  AttributeCount := 0;
  CurrentElement := nil;
  while FReader.Read do
  begin
    case FReader.NodeType of
      ntNone:
        Assert(False);

      ntElement:
        begin
          if Assigned(CurrentElement) and (AttributeCount > 0) then
            CurrentElement.SetAttributes(Attributes, AttributeCount);
          AttributeCount := 0;
          if (CurrentElement = nil) and (FRoot <> nil) then
            raise EInvalidOperation.Create(RS_READ_MULTIPLE_ROOT_NODES);
          CurrentElement := TXmlElement.Create(FReader, CurrentElement, FReader.FName);
          if (FRoot = nil) then
            FRoot := CurrentElement;
        end;

      ntEndElement:
        begin
          if Assigned(CurrentElement) then
          begin
            if (AttributeCount > 0) then
              CurrentElement.SetAttributes(Attributes, AttributeCount);
            AttributeCount := 0;
            if (CurrentElement.FChildCount > 0) then
              { Trim excess memory }
              SetLength(CurrentElement.FChildren, CurrentElement.FChildCount);
            CurrentElement := CurrentElement.Parent;
          end
          else
            raise EInvalidOperation.Create(RS_NO_END_ELEMENT);
        end;

      ntAttribute:
        begin
          if (AttributeCount >= Length(Attributes)) then
            SetLength(Attributes,AttributeCount + 8);
          Attributes[AttributeCount] := FReader.FName;
          Inc(AttributeCount);
        end;

      ntContent:
        begin
          Assert(Assigned(CurrentElement));
          CurrentElement.FContent := FReader.FValue;
        end;
    end;
  end;
end;

end.
