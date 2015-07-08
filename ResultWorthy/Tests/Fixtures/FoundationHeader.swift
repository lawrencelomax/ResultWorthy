import CoreFoundation
import CoreFoundation
import CoreGraphics
import Foundation.FoundationErrors
import Foundation.NSAffineTransform
import Foundation.NSAppleEventDescriptor
import Foundation.NSAppleEventManager
import Foundation.NSAppleScript
import Foundation.NSArchiver
import Foundation.NSArray
import Foundation.NSAttributedString
import Foundation.NSAutoreleasePool
import Foundation.NSBackgroundActivityScheduler
import Foundation.NSBundle
import Foundation.NSByteCountFormatter
import Foundation.NSByteOrder
import Foundation.NSCache
import Foundation.NSCalendar
import Foundation.NSCalendarDate
import Foundation.NSCharacterSet
import Foundation.NSClassDescription
import Foundation.NSCoder
import Foundation.NSComparisonPredicate
import Foundation.NSCompoundPredicate
import Foundation.NSConnection
import Foundation.NSData
import Foundation.NSDate
import Foundation.NSDateComponentsFormatter
import Foundation.NSDateFormatter
import Foundation.NSDateIntervalFormatter
import Foundation.NSDecimal
import Foundation.NSDecimalNumber
import Foundation.NSDictionary
import Foundation.NSDistantObject
import Foundation.NSDistributedLock
import Foundation.NSDistributedNotificationCenter
import Foundation.NSEnergyFormatter
import Foundation.NSEnumerator
import Foundation.NSError
import Foundation.NSException
import Foundation.NSExpression
import Foundation.NSExtensionContext
import Foundation.NSExtensionItem
import Foundation.NSExtensionRequestHandling
import Foundation.NSFileCoordinator
import Foundation.NSFileHandle
import Foundation.NSFileManager
import Foundation.NSFilePresenter
import Foundation.NSFileVersion
import Foundation.NSFileWrapper
import Foundation.NSFormatter
import Foundation.NSGarbageCollector
import Foundation.NSGeometry
import Foundation.NSHFSFileTypes
import Foundation.NSHTTPCookie
import Foundation.NSHTTPCookieStorage
import Foundation.NSHashTable
import Foundation.NSHost
import Foundation.NSIndexPath
import Foundation.NSIndexSet
import Foundation.NSInvocation
import Foundation.NSItemProvider
import Foundation.NSJSONSerialization
import Foundation.NSKeyValueCoding
import Foundation.NSKeyValueObserving
import Foundation.NSKeyedArchiver
import Foundation.NSLengthFormatter
import Foundation.NSLinguisticTagger
import Foundation.NSLocale
import Foundation.NSLock
import Foundation.NSMapTable
import Foundation.NSMassFormatter
import Foundation.NSMetadata
import Foundation.NSMetadataAttributes
import Foundation.NSMethodSignature
import Foundation.NSNetServices
import Foundation.NSNotification
import Foundation.NSNotificationQueue
import Foundation.NSNull
import Foundation.NSNumberFormatter
import Foundation.NSObjCRuntime
import Foundation.NSObject
import Foundation.NSObjectScripting
import Foundation.NSOperation
import Foundation.NSOrderedSet
import Foundation.NSOrthography
import Foundation.NSPathUtilities
import Foundation.NSPointerArray
import Foundation.NSPointerFunctions
import Foundation.NSPort
import Foundation.NSPortCoder
import Foundation.NSPortMessage
import Foundation.NSPortNameServer
import Foundation.NSPredicate
import Foundation.NSProcessInfo
import Foundation.NSProgress
import Foundation.NSPropertyList
import Foundation.NSProtocolChecker
import Foundation.NSProxy
import Foundation.NSRange
import Foundation.NSRegularExpression
import Foundation.NSRunLoop
import Foundation.NSScanner
import Foundation.NSScriptClassDescription
import Foundation.NSScriptCoercionHandler
import Foundation.NSScriptCommand
import Foundation.NSScriptCommandDescription
import Foundation.NSScriptExecutionContext
import Foundation.NSScriptKeyValueCoding
import Foundation.NSScriptObjectSpecifiers
import Foundation.NSScriptStandardSuiteCommands
import Foundation.NSScriptSuiteRegistry
import Foundation.NSScriptWhoseTests
import Foundation.NSSet
import Foundation.NSSortDescriptor
import Foundation.NSSpellServer
import Foundation.NSStream
import Foundation.NSString
import Foundation.NSTask
import Foundation.NSTextCheckingResult
import Foundation.NSThread
import Foundation.NSTimeZone
import Foundation.NSTimer
import Foundation.NSURL
import Foundation.NSURLAuthenticationChallenge
import Foundation.NSURLCache
import Foundation.NSURLConnection
import Foundation.NSURLCredential
import Foundation.NSURLCredentialStorage
import Foundation.NSURLDownload
import Foundation.NSURLError
import Foundation.NSURLHandle
import Foundation.NSURLProtectionSpace
import Foundation.NSURLProtocol
import Foundation.NSURLRequest
import Foundation.NSURLResponse
import Foundation.NSURLSession
import Foundation.NSUUID
import Foundation.NSUbiquitousKeyValueStore
import Foundation.NSUndoManager
import Foundation.NSUserActivity
import Foundation.NSUserDefaults
import Foundation.NSUserNotification
import Foundation.NSUserScriptTask
import Foundation.NSValue
import Foundation.NSValueTransformer
import Foundation.NSXMLDTD
import Foundation.NSXMLDTDNode
import Foundation.NSXMLDocument
import Foundation.NSXMLElement
import Foundation.NSXMLNode
import Foundation.NSXMLNodeOptions
import Foundation.NSXMLParser
import Foundation.NSXPCConnection
import Foundation.NSZone
import Foundation

extension NSArray : SequenceType {
    init(_ number: NSNumber)
    final func generate() -> NSFastGenerator
}


     
extension Double : _ObjectiveCBridgeable {
    final func generate() -> NSFastGenerator
}


extension NSArray : ArrayLiteralConvertible {

    /// Create an instance initialized with `elements`.
    required convenience init(arrayLiteral elements: AnyObject...)
}


extension NSDate : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension String {

    /// Returns an Array of the encodings string objects support
    /// in the application’s environment.
    static func availableStringEncodings() -> [NSStringEncoding]

    /// Returns the C-string encoding assumed for any method accepting
    /// a C string as an argument.
    static func defaultCStringEncoding() -> NSStringEncoding

    /// Returns a human-readable string giving the name of a given encoding.
    static func localizedNameOfStringEncoding(encoding: NSStringEncoding) -> String

    /// Returns a string created by using a given format string as a
    /// template into which the remaining argument values are substituted
    /// according to the user's default locale.
    static func localizedStringWithFormat(format: String, _ arguments: CVarArgType...) -> String

    /// Returns a string built from the strings in a given array
    /// by concatenating them with a path separator between each pair.
    static func pathWithComponents(components: [String]) -> String

    /// Produces a string created by reading data from the file at a
    /// given path interpreted using a given encoding.
    init?(contentsOfFile path: String, encoding enc: NSStringEncoding, error: NSErrorPointer = default)

    /// Produces a string created by reading data from the file at
    /// a given path and returns by reference the encoding used to
    /// interpret the file.
    init?(contentsOfFile path: String, usedEncoding: UnsafeMutablePointer<NSStringEncoding> = default, error: NSErrorPointer = default)

    /// Produces a string created by reading data from a given URL
    /// interpreted using a given encoding.  Errors are written into the
    /// inout `error` argument.
    init?(contentsOfURL url: NSURL, encoding enc: NSStringEncoding, error: NSErrorPointer = default)

    /// Produces a string created by reading data from a given URL
    /// and returns by reference the encoding used to interpret the
    /// data.  Errors are written into the inout `error` argument.
    init?(contentsOfURL url: NSURL, usedEncoding enc: UnsafeMutablePointer<NSStringEncoding> = default, error: NSErrorPointer = default)

    /// Produces a string containing the bytes in a given C array,
    /// interpreted according to a given encoding.
    init?(CString: UnsafePointer<CChar>, encoding enc: NSStringEncoding)

    /// Produces a string created by copying the data from a given
    /// C array of UTF8-encoded bytes.
    init?(UTF8String bytes: UnsafePointer<CChar>)

    /// Returns a Boolean value that indicates whether the
    /// `String` can be converted to a given encoding without loss of
    /// information.
    func canBeConvertedToEncoding(encoding: NSStringEncoding) -> Bool

    /// Produce a string with the first character from each word changed
    /// to the corresponding uppercase value.
    var capitalizedString: String { get }

    /// Returns a capitalized representation of the `String`
    /// using the specified locale.
    func capitalizedStringWithLocale(locale: NSLocale?) -> String

    /// Returns the result of invoking `compare:options:` with
    /// `NSCaseInsensitiveSearch` as the only option.
    func caseInsensitiveCompare(aString: String) -> NSComparisonResult

    /// Returns a string containing characters the `String` and a
    /// given string have in common, starting from the beginning of each
    /// up to the first characters that aren’t equivalent.
    func commonPrefixWithString(aString: String, options: NSStringCompareOptions) -> String

    /// Compares the string using the specified options and
    /// returns the lexical ordering for the range.
    func compare(aString: String, options mask: NSStringCompareOptions = default, range: Range<String.Index>? = default, locale: NSLocale? = default) -> NSComparisonResult

    /// Interprets the `String` as a path in the file system and
    /// attempts to perform filename completion, returning a numeric
    /// value that indicates whether a match was possible, and by
    /// reference the longest path that matches the `String`.
    /// Returns the actual number of matching paths.
    func completePathIntoString(_ outputName: UnsafeMutablePointer<String> = default, caseSensitive: Bool, matchesIntoArray: UnsafeMutablePointer<[String]> = default, filterTypes: [String]? = default) -> Int

    /// Returns an array containing substrings from the `String`
    /// that have been divided by characters in a given set.
    func componentsSeparatedByCharactersInSet(separator: NSCharacterSet) -> [String]

    /// Returns an array containing substrings from the `String`
    /// that have been divided by a given separator.
    func componentsSeparatedByString(separator: String) -> [String]

    /// Returns a representation of the `String` as a C string
    /// using a given encoding.
    func cStringUsingEncoding(encoding: NSStringEncoding) -> [CChar]?

    /// Returns an `NSData` object containing a representation of
    /// the `String` encoded using a given encoding.
    func dataUsingEncoding(encoding: NSStringEncoding, allowLossyConversion: Bool = default) -> NSData?

    /// Returns a string made by normalizing the `String`’s
    /// contents using Form D.
    var decomposedStringWithCanonicalMapping: String { get }

    /// Returns a string made by normalizing the `String`’s
    /// contents using Form KD.
    var decomposedStringWithCompatibilityMapping: String { get }

    /// Enumerates all the lines in a string.
    /// func enumerateLines(body: (line: String, inout stop: Bool) -> ())

    /// Performs linguistic analysis on the specified string by
    /// enumerating the specific range of the string, providing the
    /// Block with the located tags.
    /// func enumerateLinguisticTagsInRange(range: Range<String.Index>, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions, orthography: NSOrthography?, _ body: (String, Range<String.Index>, Range<String.Index>, inout Bool) -> ())

    /// Enumerates the substrings of the specified type in the
    /// specified range of the string.
    /// func enumerateSubstringsInRange(range: Range<String.Index>, options opts: NSStringEnumerationOptions, _ body: (substring: String, substringRange: Range<String.Index>, enclosingRange: Range<String.Index>, inout Bool) -> ())

    /// Returns the fastest encoding to which the `String` may be
    /// converted without loss of information.
    var fastestEncoding: NSStringEncoding { get }

    /// Returns a file system-specific representation of the `String`.
    func fileSystemRepresentation() -> [CChar]

    /// Gets a given range of characters as bytes in a specified encoding.
    /// Note: will get a maximum of `min(buffer.count, maxLength)` bytes.
    func getBytes(inout buffer: [UInt8], maxLength: Int, usedLength: UnsafeMutablePointer<Int>, encoding: NSStringEncoding, options: NSStringEncodingConversionOptions, range: Range<String.Index>, remainingRange: UnsafeMutablePointer<Range<String.Index>>) -> Bool

    /// Converts the `String`’s content to a given encoding and
    /// stores them in a buffer. Note: will store a maximum of
    /// `min(buffer.count, maxLength)` bytes.
    func getCString(inout buffer: [CChar], maxLength: Int, encoding: NSStringEncoding) -> Bool

    /// Interprets the `String` as a system-independent path and
    /// fills a buffer with a C-string in a format and encoding suitable
    /// for use with file-system calls. Note: will store a maximum of
    /// `min(buffer.count, maxLength)` bytes.
    func getFileSystemRepresentation(inout buffer: [CChar], maxLength: Int) -> Bool

    /// Returns by reference the beginning of the first line and
    /// the end of the last line touched by the given range.
    func getLineStart(start: UnsafeMutablePointer<String.Index>, end: UnsafeMutablePointer<String.Index>, contentsEnd: UnsafeMutablePointer<String.Index>, forRange: Range<String.Index>)

    /// Returns by reference the beginning of the first paragraph
    /// and the end of the last paragraph touched by the given range.
    func getParagraphStart(start: UnsafeMutablePointer<String.Index>, end: UnsafeMutablePointer<String.Index>, contentsEnd: UnsafeMutablePointer<String.Index>, forRange: Range<String.Index>)

    /// An unsigned integer that can be used as a hash table address.
    var hash: Int { get }

    /// Produces an initialized `NSString` object equivalent to the given
    /// `bytes` interpreted in the given `encoding`.
    init?<S : SequenceType where UInt8 == UInt8>(bytes: S, encoding: NSStringEncoding)

    /// Produces an initialized `String` object that contains a
    /// given number of bytes from a given buffer of bytes interpreted
    /// in a given encoding, and optionally frees the buffer.  WARNING:
    /// this initializer is not memory-safe!
    init?(bytesNoCopy bytes: UnsafeMutablePointer<Void>, length: Int, encoding: NSStringEncoding, freeWhenDone flag: Bool)

    /// Returns an initialized `String` object that contains a
    /// given number of characters from a given array of Unicode
    /// characters.
    init(utf16CodeUnits: UnsafePointer<unichar>, count: Int)

    /// Returns an initialized `String` object that contains a given
    /// number of characters from a given array of UTF-16 Code Units
    init(utf16CodeUnitsNoCopy: UnsafePointer<unichar>, count: Int, freeWhenDone flag: Bool)

    /// Returns a `String` object initialized by using a given
    /// format string as a template into which the remaining argument
    /// values are substituted.
    init(format: String, _ arguments: CVarArgType...)

    /// Returns a `String` object initialized by using a given
    /// format string as a template into which the remaining argument
    /// values are substituted according to the user’s default locale.
    init(format: String, arguments: [CVarArgType])

    /// Returns a `String` object initialized by using a given
    /// format string as a template into which the remaining argument
    /// values are substituted according to given locale information.
    init(format: String, locale: NSLocale?, _ args: CVarArgType...)

    /// Returns a `String` object initialized by using a given
    /// format string as a template into which the remaining argument
    /// values are substituted according to given locale information.
    init(format: String, locale: NSLocale?, arguments: [CVarArgType])

    /// Returns the last path component of the `String`.
    var lastPathComponent: String { get }

    /// Returns the number of Unicode characters in the `String`.
    var utf16Count: Int { get }

    /// Returns the number of bytes required to store the
    /// `String` in a given encoding.
    func lengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int

    /// Returns the range of characters representing the line or lines
    /// containing a given range.
    func lineRangeForRange(aRange: Range<String.Index>) -> Range<String.Index>

    /// Returns an array of linguistic tags for the specified
    /// range and requested tags within the receiving string.
    func linguisticTagsInRange(range: Range<String.Index>, scheme tagScheme: String, options opts: NSLinguisticTaggerOptions = default, orthography: NSOrthography? = default, tokenRanges: UnsafeMutablePointer<[Range<String.Index>]> = default) -> [String]

    /// Compares the string and a given string using a
    /// case-insensitive, localized, comparison.
    func localizedCaseInsensitiveCompare(aString: String) -> NSComparisonResult

    /// Compares the string and a given string using a localized
    /// comparison.
    func localizedCompare(aString: String) -> NSComparisonResult

    /// Compares strings as sorted by the Finder.
    func localizedStandardCompare(string: String) -> NSComparisonResult

    /// Returns a version of the string with all letters
    /// converted to lowercase, taking into account the specified
    /// locale.
    func lowercaseStringWithLocale(locale: NSLocale) -> String

    /// Returns the maximum number of bytes needed to store the
    /// `String` in a given encoding.
    func maximumLengthOfBytesUsingEncoding(encoding: NSStringEncoding) -> Int

    /// Returns the range of characters representing the
    /// paragraph or paragraphs containing a given range.
    func paragraphRangeForRange(aRange: Range<String.Index>) -> Range<String.Index>

    /// Returns an array of NSString objects containing, in
    /// order, each path component of the `String`.
    var pathComponents: [String] { get }

    /// Interprets the `String` as a path and returns the
    /// `String`’s extension, if any.
    var pathExtension: String { get }

    /// Returns a string made by normalizing the `String`’s
    /// contents using Form C.
    var precomposedStringWithCanonicalMapping: String { get }

    /// Returns a string made by normalizing the `String`’s
    /// contents using Form KC.
    var precomposedStringWithCompatibilityMapping: String { get }

    /// Parses the `String` as a text representation of a
    /// property list, returning an NSString, NSData, NSArray, or
    /// NSDictionary object, according to the topmost element.
    func propertyList() -> AnyObject

    /// Returns a dictionary object initialized with the keys and
    /// values found in the `String`.
    func propertyListFromStringsFileFormat() -> [String : String]

    /// Finds and returns the range in the `String` of the first
    /// character from a given character set found in a given range with
    /// given options.
    func rangeOfCharacterFromSet(aSet: NSCharacterSet, options mask: NSStringCompareOptions = default, range aRange: Range<String.Index>? = default) -> Range<String.Index>?

    /// Returns the range in the `String` of the composed
    /// character sequence located at a given index.
    func rangeOfComposedCharacterSequenceAtIndex(anIndex: String.Index) -> Range<String.Index>

    /// Returns the range in the string of the composed character
    /// sequences for a given range.
    func rangeOfComposedCharacterSequencesForRange(range: Range<String.Index>) -> Range<String.Index>

    /// Finds and returns the range of the first occurrence of a
    /// given string within a given range of the `String`, subject to
    /// given options, using the specified locale, if any.
    func rangeOfString(aString: String, options mask: NSStringCompareOptions = default, range searchRange: Range<String.Index>? = default, locale: NSLocale? = default) -> Range<String.Index>?

    /// Returns the smallest encoding to which the `String` can
    /// be converted without loss of information.
    var smallestEncoding: NSStringEncoding { get }

    /// Returns a new string that replaces the current home
    /// directory portion of the current path with a tilde (`~`)
    /// character.
    var stringByAbbreviatingWithTildeInPath: String { get }

    /// Returns a new string made from the `String` by replacing
    /// all characters not in the specified set with percent encoded
    /// characters.
    func stringByAddingPercentEncodingWithAllowedCharacters(allowedCharacters: NSCharacterSet) -> String?

    /// Returns a representation of the `String` using a given
    /// encoding to determine the percent escapes necessary to convert
    /// the `String` into a legal URL string.
    func stringByAddingPercentEscapesUsingEncoding(encoding: NSStringEncoding) -> String?

    /// Returns a string made by appending to the `String` a
    /// string constructed from a given format string and the following
    /// arguments.
    func stringByAppendingFormat(format: String, _ arguments: CVarArgType...) -> String

    /// Returns a new string made by appending to the `String` a given string.
    func stringByAppendingPathComponent(aString: String) -> String

    /// Returns a new string made by appending to the `String` an
    /// extension separator followed by a given extension.
    func stringByAppendingPathExtension(ext: String) -> String?

    /// Returns a new string made by appending a given string to
    /// the `String`.
    func stringByAppendingString(aString: String) -> String

    /// Returns a new string made by deleting the last path
    /// component from the `String`, along with any final path
    /// separator.
    var stringByDeletingLastPathComponent: String { get }

    /// Returns a new string made by deleting the extension (if
    /// any, and only the last) from the `String`.
    var stringByDeletingPathExtension: String { get }

    /// Returns a new string made by expanding the initial
    /// component of the `String` to its full path value.
    var stringByExpandingTildeInPath: String { get }

    /// Returns a string with the given character folding options
    /// applied.
    func stringByFoldingWithOptions(options: NSStringCompareOptions, locale: NSLocale) -> String

    /// Returns a new string formed from the `String` by either
    /// removing characters from the end, or by appending as many
    /// occurrences as necessary of a given pad string.
    func stringByPaddingToLength(newLength: Int, withString padString: String, startingAtIndex padIndex: Int) -> String

    /// Returns a new string made from the `String` by replacing
    /// all percent encoded sequences with the matching UTF-8
    /// characters.
    var stringByRemovingPercentEncoding: String? { get }

    /// Returns a new string in which the characters in a
    /// specified range of the `String` are replaced by a given string.
    func stringByReplacingCharactersInRange(range: Range<String.Index>, withString replacement: String) -> String

    /// Returns a new string in which all occurrences of a target
    /// string in a specified range of the `String` are replaced by
    /// another given string.
    func stringByReplacingOccurrencesOfString(target: String, withString replacement: String, options: NSStringCompareOptions = default, range searchRange: Range<String.Index>? = default) -> String

    /// Returns a new string made by replacing in the `String`
    /// all percent escapes with the matching characters as determined
    /// by a given encoding.
    func stringByReplacingPercentEscapesUsingEncoding(encoding: NSStringEncoding) -> String?

    /// Returns a new string made from the `String` by resolving
    /// all symbolic links and standardizing path.
    var stringByResolvingSymlinksInPath: String { get }

    /// Returns a new string made by removing extraneous path
    /// components from the `String`.
    var stringByStandardizingPath: String { get }

    /// Returns a new string made by removing from both ends of
    /// the `String` characters contained in a given character set.
    func stringByTrimmingCharactersInSet(set: NSCharacterSet) -> String

    /// Returns an array of strings made by separately appending
    /// to the `String` each string in in a given array.
    func stringsByAppendingPaths(paths: [String]) -> [String]

    /// Returns a new string containing the characters of the
    /// `String` from the one at a given index to the end.
    func substringFromIndex(index: String.Index) -> String

    /// Returns a new string containing the characters of the
    /// `String` up to, but not including, the one at a given index.
    func substringToIndex(index: String.Index) -> String

    /// Returns a string object containing the characters of the
    /// `String` that lie within a given range.
    func substringWithRange(aRange: Range<String.Index>) -> String

    /// Returns a version of the string with all letters
    /// converted to uppercase, taking into account the specified
    /// locale.
    func uppercaseStringWithLocale(locale: NSLocale) -> String

    /// Writes the contents of the `String` to a file at a given
    /// path using a given encoding.
    func writeToFile(path: String, atomically useAuxiliaryFile: Bool, encoding enc: NSStringEncoding, error: NSErrorPointer = default) -> Bool

    /// Writes the contents of the `String` to the URL specified
    /// by url using the specified encoding.
    func writeToURL(url: NSURL, atomically useAuxiliaryFile: Bool, encoding enc: NSStringEncoding, error: NSErrorPointer = default) -> Bool
}

extension String {
    var lowercaseString: String { get }
    var uppercaseString: String { get }
}

extension NSObject : CVarArgType {

    /// Transform `self` into a series of machine words that can be
    /// appropriately interpreted by C varargs
    func encode() -> [Word]
}

extension NSObject : Printable {
}

extension NSObject : Equatable, Hashable {

    /// The hash value.
    ///
    /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
    ///
    /// **Note:** the hash value is not guaranteed to be stable across
    /// different invocations of the same program.  Do not persist the
    /// hash value across program runs.
    var hashValue: Int { get }
}

extension NSString : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension NSString {
    convenience init(format: NSString, _ args: CVarArgType...)
    convenience init(format: NSString, locale: NSLocale?, _ args: CVarArgType...)
    class func localizedStringWithFormat(format: NSString, _ args: CVarArgType...) -> Self
    func stringByAppendingFormat(format: NSString, _ args: CVarArgType...) -> NSString
}

extension NSString : _CocoaStringType {
}

extension String : _ObjectiveCBridgeable {
}

extension NSString : StringLiteralConvertible {

    /// Create an instance initialized to `value`.
    required convenience init(unicodeScalarLiteral value: StaticString)
    required convenience init(extendedGraphemeClusterLiteral value: StaticString)

    /// Create an instance initialized to `value`.
    required convenience init(stringLiteral value: StaticString)
}

extension NSDictionary : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension String {
    init(_ cocoaString: NSString)
}

extension Float : _ObjectiveCBridgeable {
    init(_ number: NSNumber)
}

extension NSDictionary {
    convenience init(objectsAndKeys objects: AnyObject...)
}

extension Dictionary : _ObjectiveCBridgeable {
}

extension NSOrderedSet {
    convenience init(objects elements: AnyObject...)
}

extension NSDictionary : SequenceType {
    final class Generator : GeneratorType {
        func next() -> (key: AnyObject, value: AnyObject)?
    }

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// Complexity: O(1)
    func generate() -> NSDictionary.Generator
}

extension NSExpression {
    convenience init(format expressionFormat: String, _ args: CVarArgType...)
}

extension NSSet : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension NSSet {
    convenience init(objects elements: AnyObject...)
}

extension NSSet : SequenceType {

    /// Return a *generator* over the elements of this *sequence*.
    ///
    /// Complexity: O(1)
    func generate() -> NSFastGenerator
}

extension NSDictionary : DictionaryLiteralConvertible {
    required convenience init(dictionaryLiteral elements: (NSCopying, AnyObject)...)
}

extension NSNumber : FloatLiteralConvertible, IntegerLiteralConvertible, BooleanLiteralConvertible {

    /// Create an instance initialized to `value`.
    required convenience init(integerLiteral value: Int)

    /// Create an instance initialized to `value`.
    required convenience init(floatLiteral value: Double)

    /// Create an instance initialized to `value`.
    required convenience init(booleanLiteral value: Bool)
}

extension NSFastEnumerationState {
    init()
}

extension NSURL : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension NSPredicate {
    convenience init?(format predicateFormat: String, _ args: CVarArgType...)
}

extension CGFloat : _ObjectiveCBridgeable {
    init(_ number: NSNumber)
}

//extension _NSRange : _ObjectiveCBridgeable {
//}

extension _NSRange : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension _NSRange {
    init(_ x: Range<Int>)
    func toRange() -> Range<Int>?
}

extension NSArray {
    convenience init(objects elements: AnyObject...)
}

extension Bool : _ObjectiveCBridgeable {
    init(_ number: NSNumber)
}

extension NSArray : Reflectable {

    /// Returns a mirror that reflects `self`.
    func getMirror() -> MirrorType
}

extension UInt : _ObjectiveCBridgeable {
    init(_ number: NSNumber)
}

//extension NSMutableString {
//    func appendFormat(format: NSString, _ args: CVarArgType...)
//}

extension Int : _ObjectiveCBridgeable {
    init(_ number: NSNumber)
}

func !=(lhs: String, rhs: NSString) -> Bool

func !=(lhs: NSString, rhs: String) -> Bool

func !=(lhs: NSString, rhs: NSString) -> Bool

func <(lhs: NSString, rhs: String) -> Bool

func <(lhs: String, rhs: NSString) -> Bool

func <=(lhs: String, rhs: NSString) -> Bool

func <=(lhs: NSString, rhs: String) -> Bool

func ==(lhs: NSString, rhs: NSString) -> Bool

func ==(lhs: NSString, rhs: String) -> Bool

func ==(lhs: String, rhs: NSString) -> Bool

func ==(lhs: NSObject, rhs: NSObject) -> Bool

func >(lhs: NSString, rhs: String) -> Bool

func >(lhs: String, rhs: NSString) -> Bool

func >=(lhs: NSString, rhs: String) -> Bool

func >=(lhs: String, rhs: NSString) -> Bool

let NSASCIIStringEncoding: UInt

typealias NSErrorPointer = AutoreleasingUnsafeMutablePointer<NSError?>

final class NSFastGenerator : GeneratorType {
    func next() -> AnyObject?
    init(_ enumerable: NSFastEnumeration)
}

let NSISO2022JPStringEncoding: UInt

let NSISOLatin1StringEncoding: UInt

let NSISOLatin2StringEncoding: UInt

let NSJapaneseEUCStringEncoding: UInt


/// Returns a localized string, using the main bundle if one is not specified.
func NSLocalizedString(key: String, tableName: String? = default, bundle: NSBundle = default, value: String = default, #comment: String) -> String

func NSLog(format: String, args: CVarArgType...)

let NSMacOSRomanStringEncoding: UInt

var NSMaxXEdge: NSRectEdge { get }

var NSMaxYEdge: NSRectEdge { get }

var NSMinXEdge: NSRectEdge { get }

var NSMinYEdge: NSRectEdge { get }

let NSNEXTSTEPStringEncoding: UInt

let NSNonLossyASCIIStringEncoding: UInt

let NSNotFound: Int

let NSShiftJISStringEncoding: UInt

typealias NSStringEncoding = UInt

let NSSymbolStringEncoding: UInt

let NSUTF16BigEndianStringEncoding: UInt

let NSUTF16LittleEndianStringEncoding: UInt

let NSUTF16StringEncoding: UInt

let NSUTF32BigEndianStringEncoding: UInt

let NSUTF32LittleEndianStringEncoding: UInt

let NSUTF32StringEncoding: UInt

let NSUTF8StringEncoding: UInt

let NSUnicodeStringEncoding: UInt

let NSWindowsCP1250StringEncoding: UInt

let NSWindowsCP1251StringEncoding: UInt

let NSWindowsCP1252StringEncoding: UInt

let NSWindowsCP1253StringEncoding: UInt

let NSWindowsCP1254StringEncoding: UInt

let kCFStringEncodingASCII: CFStringEncoding

