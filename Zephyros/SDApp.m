//
//  SDAppProxy.m
//  Zephyros
//
//  Created by Steven on 4/21/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDApp.h"

#import "SDWindow.h"
#import "SDUniversalAccessHelper.h"

#import "SDWindow.h"

#import "SDAppStalker.h"

@interface SDApp ()

@property AXUIElementRef app;
@property (readwrite) pid_t pid;
@property AXObserverRef observer;

- (id) initWithElement:(AXUIElementRef)element;

@end

void sendNotificationButNotTooOften(NSString* name, id thing) {
    NSNotification* note = [NSNotification notificationWithName:name object:nil userInfo:@{@"thing": thing}];
    [[NSNotificationQueue defaultQueue] enqueueNotification:note postingStyle:NSPostNow];
}

void obsessiveWindowCallback(AXObserverRef observer, AXUIElementRef element, CFStringRef notification, void *refcon) {
    if (CFEqual(notification, kAXWindowCreatedNotification)) {
        SDWindow* window = [[SDWindow alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventWindowCreated, window);
    }
    else if (CFEqual(notification, kAXWindowMovedNotification)) {
        SDWindow* window = [[SDWindow alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventWindowMoved, window);
    }
    else if (CFEqual(notification, kAXWindowResizedNotification)) {
        SDWindow* window = [[SDWindow alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventWindowResized, window);
    }
    else if (CFEqual(notification, kAXWindowMiniaturizedNotification)) {
        SDWindow* window = [[SDWindow alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventWindowMinimized, window);
    }
    else if (CFEqual(notification, kAXWindowDeminiaturizedNotification)) {
        SDWindow* window = [[SDWindow alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventWindowUnminimized, window);
    }
    else if (CFEqual(notification, kAXApplicationHiddenNotification)) {
        SDApp* app = [[SDApp alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventAppHidden, app);
    }
    else if (CFEqual(notification, kAXApplicationShownNotification)) {
        SDApp* app = [[SDApp alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventAppShown, app);
    }
    else if (CFEqual(notification, kAXFocusedWindowChangedNotification)) {
        SDWindow* window = [[SDWindow alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventFocusChanged, window);
    }
    else if (CFEqual(notification, kAXMainWindowChangedNotification)) {
        SDWindow* window = [[SDWindow alloc] initWithElement:element];
        sendNotificationButNotTooOften(SDListenEventFocusChanged, window);
    }
    else if (CFEqual(notification, kAXApplicationActivatedNotification)) {
        SDWindow* window = [SDWindow focusedWindow];
        if (window)
            sendNotificationButNotTooOften(SDListenEventFocusChanged, window);
    }
}

@implementation SDApp

+ (NSArray*) runningApps {
    if ([SDUniversalAccessHelper complainIfNeeded])
        return nil;
    
    NSMutableArray* apps = [NSMutableArray array];
    
    for (NSRunningApplication* runningApp in [[NSWorkspace sharedWorkspace] runningApplications]) {
        SDApp* app = [[SDApp alloc] initWithPID:[runningApp processIdentifier]];
        [apps addObject:app];
    }
    
    return apps;
}

- (id) initWithElement:(AXUIElementRef)element {
    pid_t pid;
    AXUIElementGetPid(element, &pid);
    return [self initWithPID:pid];
}

- (id) initWithRunningApp:(NSRunningApplication*)app {
    return [self initWithPID:[app processIdentifier]];
}

- (id) initWithPID:(pid_t)pid {
    if (self = [super init]) {
        self.pid = pid;
        self.app = AXUIElementCreateApplication(pid);
    }
    return self;
}

- (void) dealloc {
    if (self.app)
        CFRelease(self.app);
}

- (NSArray*) visibleWindows {
    if ([SDUniversalAccessHelper complainIfNeeded])
        return nil;
    
    return [[self allWindows] filteredArrayUsingPredicate:[NSPredicate predicateWithBlock:^BOOL(SDWindow* win, NSDictionary *bindings) {
        return ![[win app] isHidden]
        && ![win isWindowMinimized]
        && [win isNormalWindow];
    }]];
}

- (NSArray*) allWindows {
    NSMutableArray* windows = [NSMutableArray array];
    
    CFArrayRef _windows;
    AXError result = AXUIElementCopyAttributeValues(self.app, kAXWindowsAttribute, 0, 100, &_windows);
    if (result == kAXErrorSuccess) {
        for (NSInteger i = 0; i < CFArrayGetCount(_windows); i++) {
            AXUIElementRef win = CFArrayGetValueAtIndex(_windows, i);
            
            SDWindow* window = [[SDWindow alloc] initWithElement:win];
            [windows addObject:window];
        }
        CFRelease(_windows);
    }
    
    return windows;
}

- (BOOL) isHidden {
    CFTypeRef _isHidden;
    NSNumber* isHidden = @NO;
    if (AXUIElementCopyAttributeValue(self.app, (CFStringRef)NSAccessibilityHiddenAttribute, (CFTypeRef *)&_isHidden) == kAXErrorSuccess) {
        isHidden = CFBridgingRelease(_isHidden);
    }
    return [isHidden boolValue];
}

- (void) show {
    [self setAppProperty:NSAccessibilityHiddenAttribute withValue:[NSNumber numberWithLong:NO]];
}

- (void) hide {
    [self setAppProperty:NSAccessibilityHiddenAttribute withValue:[NSNumber numberWithLong:YES]];
}

- (NSString*) title {
    return [[NSRunningApplication runningApplicationWithProcessIdentifier:self.pid] localizedName];
}

- (void) kill {
    [[NSRunningApplication runningApplicationWithProcessIdentifier:self.pid] terminate];
}

- (void) kill9 {
    [[NSRunningApplication runningApplicationWithProcessIdentifier:self.pid] forceTerminate];
}

- (void) startObservingStuff {
    AXObserverRef observer;
    AXError err = AXObserverCreate(self.pid, obsessiveWindowCallback, &observer);
    if (err != kAXErrorSuccess) {
//        NSLog(@"start observing stuff failed at point #1 with: %d", err);
        return;
    }
    
    self.observer = observer;
    AXObserverAddNotification(self.observer, self.app, kAXWindowCreatedNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXWindowMovedNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXWindowResizedNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXWindowMiniaturizedNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXWindowDeminiaturizedNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXApplicationHiddenNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXApplicationShownNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXFocusedWindowChangedNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXApplicationActivatedNotification, NULL);
    AXObserverAddNotification(self.observer, self.app, kAXMainWindowChangedNotification, NULL);
    
    CFRunLoopAddSource([[NSRunLoop currentRunLoop] getCFRunLoop],
                       AXObserverGetRunLoopSource(self.observer),
                       kCFRunLoopDefaultMode);
}

- (void) stopObservingStuff {
    CFRunLoopRemoveSource([[NSRunLoop currentRunLoop] getCFRunLoop],
                          AXObserverGetRunLoopSource(self.observer),
                          kCFRunLoopDefaultMode);
    
    AXObserverRemoveNotification(self.observer, self.app, kAXWindowCreatedNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXWindowMovedNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXWindowResizedNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXWindowMiniaturizedNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXWindowDeminiaturizedNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXApplicationHiddenNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXApplicationShownNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXFocusedWindowChangedNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXApplicationActivatedNotification);
    AXObserverRemoveNotification(self.observer, self.app, kAXMainWindowChangedNotification);
    
    CFRelease(self.observer);
    self.observer = nil;
}

- (id) getAppProperty:(NSString*)propType withDefaultValue:(id)defaultValue {
    CFTypeRef _someProperty;
    if (AXUIElementCopyAttributeValue(self.app, (__bridge CFStringRef)propType, &_someProperty) == kAXErrorSuccess)
        return CFBridgingRelease(_someProperty);
    
    return defaultValue;
}

- (BOOL) setAppProperty:(NSString*)propType withValue:(id)value {
    AXError result = AXUIElementSetAttributeValue(self.app, (__bridge CFStringRef)(propType), (__bridge CFTypeRef)(value));
    return result == kAXErrorSuccess;
}

@end
