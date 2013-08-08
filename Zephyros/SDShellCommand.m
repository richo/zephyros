//
//  SDShellCommand.m
//  Zephyros
//
//  Created by Steven Degutis on 8/3/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDShellCommand.h"

@interface SDShellCommand ()

@property NSTask* task;
@property NSTask* taskLifeWatcher;

@end

@implementation SDShellCommand

- (void) launch {
    NSPipe* stdoutPipe = [NSPipe pipe];
    NSPipe* stderrPipe = [NSPipe pipe];
    
    self.task = [[NSTask alloc] init];
    
    [self.task setLaunchPath:@"/bin/bash"];
    [self.task setArguments:@[@"-l", @"-c", self.cmd]];
    
    [self.task setStandardOutput:stdoutPipe];
    [self.task setStandardError:stderrPipe];
    
    self.taskLifeWatcher =
    [[NSNotificationCenter defaultCenter] addObserverForName:NSTaskDidTerminateNotification
                                                      object:self.task
                                                       queue:nil
                                                  usingBlock:^(NSNotification *note) {
                                                      [self taskDied];
                                                  }];
    
    if (self.gotStdout)
        [stdoutPipe fileHandleForReading].readabilityHandler = self.gotStdout;
    
    if (self.gotStderr)
        [stderrPipe fileHandleForReading].readabilityHandler = self.gotStderr;
    
    [self.task launch];
}

- (void) kill {
    pid_t p = [self.task processIdentifier];
    [self.task terminate];
    kill(p, SIGKILL);
    self.task = nil;
}

- (BOOL) isRunning {
    return [self.task isRunning];
}

- (void) taskDied {
    [[NSNotificationCenter defaultCenter] removeObserver:self.taskLifeWatcher];
    
    if (self.died)
        self.died();
}

@end
