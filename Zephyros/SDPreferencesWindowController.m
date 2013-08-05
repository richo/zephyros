//
//  SDConfigChooserWindowController.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDPreferencesWindowController.h"

#import "SDConfigLauncher.h"

#import "SDWatchedPathsWindowController.h"

#import "SDClientListener.h"

@interface SDPreferencesWindowController ()

@property SDWatchedPathsWindowController* pathsController;

@property BOOL scriptRunning;

@end


@interface SDStartOrStopScriptTitleTransformer : NSValueTransformer
@end
@implementation SDStartOrStopScriptTitleTransformer
+ (Class)transformedValueClass { return [NSString class]; }
+ (BOOL)allowsReverseTransformation { return NO; }
- (id)transformedValue:(id)value {
    if ([value boolValue])
        return @"Kill";
    else
        return @"Run";
}
@end


@interface SDScriptRunningImageTransformer : NSValueTransformer
@end
@implementation SDScriptRunningImageTransformer
+ (Class)transformedValueClass { return [NSString class]; }
+ (BOOL)allowsReverseTransformation { return NO; }
- (id)transformedValue:(id)value {
    if ([value boolValue])
        return [NSImage imageNamed:NSImageNameStatusAvailable];
    else
        return [NSImage imageNamed:NSImageNameStatusUnavailable];
}
@end



@implementation SDPreferencesWindowController

+ (SDPreferencesWindowController*) sharedConfigChooserWindowController {
    static SDPreferencesWindowController* sharedConfigChooserWindowController;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedConfigChooserWindowController = [[SDPreferencesWindowController alloc] init];
    });
    return sharedConfigChooserWindowController;
}

- (id) init {
    if (self = [super init]) {
        [[NSNotificationCenter defaultCenter] addObserverForName:SDScriptLaunchedNotification
                                                          object:nil
                                                           queue:nil
                                                      usingBlock:^(NSNotification *note) {
                                                          self.scriptRunning = YES;
                                                      }];
        
        [[NSNotificationCenter defaultCenter] addObserverForName:SDScriptDiedNotification
                                                          object:nil
                                                           queue:nil
                                                      usingBlock:^(NSNotification *note) {
                                                          self.scriptRunning = NO;
                                                      }];
    }
    return self;
}

- (NSString*) windowNibName {
    return @"SDPreferencesWindow";
}

- (IBAction) startOrStopScript:(id)sender {
    [[SDConfigLauncher sharedConfigLauncher] startOrStopScript];
}

- (void) show {
    self.scriptRunning = [SDConfigLauncher sharedConfigLauncher].isRunning;
    
    [[self window] center];
    [self showWindow:self];
}

- (IBAction) changeIfRunsScript:(id)sender {
    if ([sender state] == NSOffState) {
        [[SDConfigLauncher sharedConfigLauncher] unlaunch];
    }
}

- (IBAction) changeWhetherWatchingPaths:(id)sender {
//    [[SDConfigLauncher sharedConfigLauncher] watchPathsMaybe];
}

- (IBAction) switchSocketType:(id)sender {
    [[SDClientListener sharedListener] startListening];
}

- (IBAction) switchSocketPort:(id)sender {
    [[SDClientListener sharedListener] startListening];
}

- (BOOL)control:(NSControl *)control textView:(NSTextView *)textView doCommandBySelector:(SEL)command {
    if (command == @selector(insertNewline:)) {
        [textView insertNewlineIgnoringFieldEditor:self];
        return YES;
    }
    return NO;
}

- (IBAction) changePaths:(id)sender {
    self.pathsController = [[SDWatchedPathsWindowController alloc] init];
    [NSApp beginSheet:[self.pathsController window]
       modalForWindow:[self window]
        modalDelegate:self
       didEndSelector:@selector(pathsSheetDidEnd:returnCode:contextInfo:)
          contextInfo:NULL];
}

- (void) pathsSheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo {
    [sheet orderOut:self];
    
    if (returnCode) {
        [[NSUserDefaults standardUserDefaults] setObject:self.pathsController.pathsToWatch
                                                  forKey:SDRelaunchPathsDefaultsKey];
        
        [[SDConfigLauncher sharedConfigLauncher] watchPathsMaybe];
    }
    
    self.pathsController = nil;
}

@end
