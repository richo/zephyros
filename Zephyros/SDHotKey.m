//
//  BindkeyOp.m
//  Zephyros
//
//  Created by Steven on 4/13/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDHotKey.h"

#import "MASShortcut+Monitoring.h"
#import "SDKeyBindingTranslator.h"


@interface SDHotKey ()

@property id internalHandler;

@end

@implementation SDHotKey

- (MASShortcut*) shortcutObject {
    NSUInteger code = [SDKeyBindingTranslator keyCodeForString:self.key];
    NSUInteger mods = [SDKeyBindingTranslator modifierFlagsForStrings:self.modifiers];
    
    return [MASShortcut shortcutWithKeyCode:code modifierFlags:mods];
}

- (NSString*) hotKeyDescription {
    return [NSString stringWithFormat:@"%@ %@",
            [self.modifiers componentsJoinedByString:@"-"],
            self.key];
}

- (BOOL) bind {
    self.internalHandler = [MASShortcut addGlobalHotkeyMonitorWithShortcut:[self shortcutObject] handler:^{
        self.fn();
    }];
    return self.internalHandler != nil;
}

- (void) unbind {
    [MASShortcut removeGlobalHotkeyMonitor:self.internalHandler];
}

@end
