//
//  SDWatchedPathsWindowController.m
//  Zephyros
//
//  Created by Steven Degutis on 8/1/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDWatchedPathsWindowController.h"

#import "SDPreferencesWindowController.h"

@implementation SDWatchedPathsWindowController

- (NSString*) windowNibName {
    return @"SDWatchedPathsWindow";
}

- (void)windowDidLoad {
    [super windowDidLoad];
    self.pathsToWatch = [[NSUserDefaults standardUserDefaults] stringForKey:SDRelaunchPathsDefaultsKey];
}

- (IBAction) cancel:(id)sender {
    [NSApp endSheet:[self window] returnCode:0];
}

- (IBAction) useThese:(id)sender {
    [NSApp endSheet:[self window] returnCode:1];
}

- (BOOL)control:(NSControl *)control textView:(NSTextView *)textView doCommandBySelector:(SEL)command {
    if (command == @selector(insertNewline:)) {
        [textView insertNewlineIgnoringFieldEditor:self];
        return YES;
    }
    return NO;
}

@end
