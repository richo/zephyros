//
//  SDConfigWatcher.m
//  Zephyros
//
//  Created by Steven on 7/28/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDConfigWatcher.h"

#import "SDConfigLoader.h"

@interface SDConfigWatcher ()

@property BOOL watching;
@property FSEventStreamRef stream;

@end



void fsEventsCallback(ConstFSEventStreamRef streamRef, void *clientCallBackInfo, size_t numEvents, void *eventPaths, const FSEventStreamEventFlags eventFlags[], const FSEventStreamEventId eventIds[])
{
//    [[SDConfigLoader sharedConfigLoader] reloadConfigIfWatchEnabled];
}



@implementation SDConfigWatcher

- (void) stopWatching {
    if (!self.watching)
        return;
    
    FSEventStreamStop(self.stream);
    FSEventStreamInvalidate(self.stream);
    FSEventStreamRelease(self.stream);
}

- (void) startWatching:(NSString*)path {
    self.watching = YES;
    
    NSArray *pathsToWatch = @[path];
    FSEventStreamContext context;
    context.info = NULL;
    context.version = 0;
    context.retain = NULL;
    context.release = NULL;
    context.copyDescription = NULL;
    self.stream = FSEventStreamCreate(NULL,
                                      fsEventsCallback,
                                      &context,
                                      (__bridge CFArrayRef)pathsToWatch,
                                      kFSEventStreamEventIdSinceNow,
                                      0.4,
                                      kFSEventStreamCreateFlagWatchRoot | kFSEventStreamCreateFlagNoDefer | kFSEventStreamCreateFlagFileEvents);
    FSEventStreamScheduleWithRunLoop(self.stream, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
    FSEventStreamStart(self.stream);
}

@end
