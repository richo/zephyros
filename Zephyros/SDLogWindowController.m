//
//  SDConfigProblemReporter.m
//  Zephyros
//
//  Created by Steven on 4/13/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDLogWindowController.h"

#import <WebKit/WebKit.h>

#import "SDConfigLauncher.h"



@interface SDLogTypeTransformer : NSValueTransformer
@end
@implementation SDLogTypeTransformer
+ (Class)transformedValueClass { return [NSImage self]; }
+ (BOOL)allowsReverseTransformation { return NO; }
- (id)transformedValue:(id)value {
    if ([value isEqual:SDLogMessageTypeError])
        return [NSImage imageNamed:NSImageNameStatusUnavailable];
    if ([value isEqual:SDLogMessageTypeUser])
        return [NSImage imageNamed:NSImageNameStatusPartiallyAvailable];
    if ([value isEqual:SDLogMessageTypeRequest])
        return [NSImage imageNamed:NSImageNameStatusNone];
    if ([value isEqual:SDLogMessageTypeResponse])
        return [NSImage imageNamed:NSImageNameStatusAvailable];
    return nil;
}
@end



@interface SDLog : NSObject
@property NSString* type;
@property NSDate* time;
@property NSString* message;
@end
@implementation SDLog
@end


@interface SDLogWindowController ()

@property IBOutlet NSTableView* logTableView;
@property NSMutableArray* logs;

@end

@implementation SDLogWindowController

+ (SDLogWindowController*) sharedLogWindowController {
    static SDLogWindowController* sharedMessageWindowController;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedMessageWindowController = [[SDLogWindowController alloc] init];
        sharedMessageWindowController.logs = [NSMutableArray array];
    });
    return sharedMessageWindowController;
}

- (NSString*) windowNibName {
    return @"LogWindow";
}

- (IBAction) clearLog:(id)sender {
    [self willChangeValueForKey:@"logs"];
    [self.logs removeAllObjects];
    [self didChangeValueForKey:@"logs"];
}

- (CGFloat)tableView:(NSTableView *)tableView heightOfRow:(NSInteger)row {
    SDLog* log = [self.logs objectAtIndex:row];
    NSArray* lines = [log.message componentsSeparatedByString:@"\n"];
    NSUInteger numRows = MAX(1, [lines count] - 1);
    CGFloat normalHeight = [tableView rowHeight];
    return normalHeight * (CGFloat)numRows;
}

- (IBAction) copy:(id)sender {
    NSIndexSet* indices = [self.logTableView selectedRowIndexes];
    NSArray* selectedLogs = [self.logs objectsAtIndexes:indices];
    
    NSDateFormatter* formatter = [[NSDateFormatter alloc] init];
    [formatter setDateFormat:@"HH:mm:ss"];
    
    NSMutableString* toCopy = [NSMutableString string];
    
    for (SDLog* log in selectedLogs) {
        [toCopy appendFormat:@"%@ - %@ - %@\n", log.type, [formatter stringFromDate:log.time], log.message];
    }
    
    NSPasteboard* pasteBoard = [NSPasteboard generalPasteboard];
    
    [pasteBoard declareTypes:[NSArray arrayWithObject:NSStringPboardType] owner:nil];
    [pasteBoard setString:toCopy forType:NSStringPboardType];
}

- (void) windowDidLoad {
    self.window.level = NSFloatingWindowLevel;
    [[self window] center];
}

- (void) log:(NSString*)message type:(NSString*)type {
    SDLog* log = [[SDLog alloc] init];
    
    log.time = [NSDate date];
    log.type = type;
    log.message = message;
    
    [self.logs addObject:log];
    
    [[self class] cancelPreviousPerformRequestsWithTarget:self selector:@selector(refreshLogs:) object:nil];
    [self performSelector:@selector(refreshLogs:) withObject:nil afterDelay:0.1];
}

- (void) refreshLogs:(id)alwaysNil {
    [self willChangeValueForKey:@"logs"];
    [self didChangeValueForKey:@"logs"];
    
    NSInteger lastRow = [self.logs count] - 1;
    [self.logTableView scrollRowToVisible:lastRow];
}

- (void) show:(NSString*)message type:(NSString*)type {
    if (!self.window.isVisible) {
        [self showWindow:nil];
    }
    
    [self log:message type:type];
}

@end
