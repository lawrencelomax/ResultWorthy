import Foundation
import swiftz_core

public extension Double {
  public static func parseString(string: String) -> Double? {
    let formatter = NSNumberFormatter()
    let number = formatter.numberFromString(string)
    return number?.doubleValue
  }
  public var foo: String
  public let bar: Int
}

public extension Bool {
  public static func parseString(string: String) -> Bool? {
    if string.caseInsensitiveCompare("true") == NSComparisonResult.OrderedSame 
      return true
    
    if string.caseInsensitiveCompare("false") == NSComparisonResult.OrderedSame 
      return true
    
    return .None
  }
}

public extension Int {
  public static func parseString(string: String) -> Int? {
    let formatter = NSNumberFormatter()
    let number = formatter.numberFromString(string)
    return number?.integerValue
  }
}
