//
//  SDTopLevelClientProxy.m
//  Zephyros
//
//  Created by Steven Degutis on 8/12/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDTopLevelClientProxy.h"

#import "SDHotKey.h"
#import "SDEventListener.h"

#import "SDFuzzyMatcher.h"
#import "SDConfigLauncher.h"
#import "SDAlertWindowController.h"
#import "SDLogWindowController.h"

#import "SDWindowProxy.h"

@interface SDTopLevelClientProxy ()

@property NSMutableArray* hotkeys;
@property NSMutableArray* listeners;

@end

@implementation SDTopLevelClientProxy

- (id) init {
    if (self = [super init]) {
        self.hotkeys = [NSMutableArray array];
        self.listeners = [NSMutableArray array];
    }
    return self;
}

- (void) destroy {
    for (SDHotKey* hotkey in self.hotkeys) {
        [hotkey unbind];
    }
    
    for (SDEventListener* listener in self.listeners) {
        [listener stopListening];
    }
}

- (id) bind:(NSArray*)args msgID:(id)msgID {
//    [self check:args forTypes:@[[NSString self], [NSArray self]] inMethod:_cmd];
    
    SDHotKey* hotkey = [[SDHotKey alloc] init];
    hotkey.key = [[args objectAtIndex:0] uppercaseString];
    hotkey.modifiers = [[args objectAtIndex:1] valueForKeyPath:@"uppercaseString"];
    hotkey.fn = ^{
        [self.client sendResponse:nil forID:msgID];
    };
    
    if ([hotkey bind]) {
        [self.hotkeys addObject:hotkey];
    }
    else {
        [self.client showAPIError:[@"Can't bind: " stringByAppendingString: [hotkey hotKeyDescription]]];
    }
    
    return @-1;
}

- (id) unbind:(NSArray*)args msgID:(id)msgID {
    NSString* key = [[args objectAtIndex:0] uppercaseString];
    NSArray* modifiers = [[args objectAtIndex:1] valueForKeyPath:@"uppercaseString"];
    
    SDHotKey* foundHotkey;
    for (SDHotKey* existingHotkey in self.hotkeys) {
        if ([existingHotkey.key isEqual: key] && [existingHotkey.modifiers isEqual: modifiers]) {
            foundHotkey = existingHotkey;
            break;
        }
    }
    
    if (foundHotkey) {
        [foundHotkey unbind];
        [self.hotkeys removeObject:foundHotkey];
        return @YES;
    }
    else {
        return @NO;
    }
}

- (id) listen:(NSArray*)args msgID:(id)msgID {
    SDEventListener* listener = [[SDEventListener alloc] init];
    listener.eventName = [args objectAtIndex:0];
    listener.fn = ^(id thing) {
        [self.client sendResponse:thing forID:msgID];
    };
    
    [listener startListening];
    [self.listeners addObject:listener];
    
    return @-1;
}

- (id) relaunch_config:(NSArray*)args msgID:(id)msgID {
    [[SDConfigLauncher sharedConfigLauncher] launchConfigMaybe];
    return nil;
}

- (id) update_settings:(NSArray*)args msgID:(id)msgID {
    NSDictionary* settings = [args objectAtIndex:0];
    
    NSNumber* shouldAnimate = [settings objectForKey:@"alert_should_animate"];
    if ([shouldAnimate isKindOfClass: [NSNumber self]])
        [SDAlerts sharedAlerts].alertAnimates = [shouldAnimate boolValue];
    
    NSNumber* defaultDuration = [settings objectForKey:@"alert_default_delay"];
    if ([defaultDuration isKindOfClass: [NSNumber self]])
        [SDAlerts sharedAlerts].alertDisappearDelay = [defaultDuration doubleValue];
    
    return nil;
}

- (id) clipboard_contents:(NSArray*)args msgID:(id)msgID {
    return [[NSPasteboard generalPasteboard] stringForType:NSPasteboardTypeString];
}

- (id) focused_window:(NSArray*)args msgID:(id)msgID {
    return [SDWindowProxy focusedWindow];
}

- (id) visible_windows:(NSArray*)args msgID:(id)msgID {
    return [SDWindowProxy visibleWindows];
}

- (id) all_windows:(NSArray*)args msgID:(id)msgID {
    return [SDWindowProxy allWindows];
}

- (id) main_screen:(NSArray*)args msgID:(id)msgID {
    return [SDScreenProxy mainScreen];
}

- (id) all_screens:(NSArray*)args msgID:(id)msgID {
    return [SDScreenProxy allScreens];
}

- (id) running_apps:(NSArray*)args msgID:(id)msgID {
    return [SDAppProxy runningApps];
}

- (id) log:(NSArray*)args msgID:(id)msgID {
    [[SDLogWindowController sharedLogWindowController] show:[args objectAtIndex:0]
                                                       type:SDLogMessageTypeUser];
    return nil;
}

- (id) alert:(NSArray*)args msgID:(id)msgID {
    NSString* msg = [args objectAtIndex:0];
    NSNumber* duration = [args objectAtIndex:1];
    
    if (duration == nil || [duration isEqual: [NSNull null]]) {
        [[SDAlerts sharedAlerts] show:msg];
    }
    else {
        [[SDAlerts sharedAlerts] show:msg
                             duration:[duration doubleValue]];
    }
    
    return nil;
}

- (id) choose_from:(NSArray*)args msgID:(id)msgID {
    NSArray* list = [args objectAtIndex:0];
    NSString* title = [args objectAtIndex:1];
    NSNumber* lines = [args objectAtIndex:2];
    NSNumber* chars = [args objectAtIndex:3];
    
    [NSApp activateIgnoringOtherApps:YES];
    [SDFuzzyMatcher showChoices:list
                      charsWide:[chars intValue]
                      linesTall:[lines intValue]
                    windowTitle:title
                  choseCallback:^(long chosenIndex) {
                      [NSApp hide:self];
                      [self.client sendResponse:@(chosenIndex) forID:msgID];
                  }
               canceledCallback:^{
                   [NSApp hide:self];
                   [self.client sendResponse:[NSNull null] forID:msgID];
               }];
    return @1;
}

@end
