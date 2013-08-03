//
//  SDShellCommand.h
//  Zephyros
//
//  Created by Steven Degutis on 8/3/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDShellCommand : NSObject

@property NSString* cmd;
@property (copy) void(^gotStdout)(NSFileHandle* handle);
@property (copy) void(^gotStderr)(NSFileHandle* handle);
@property (copy) dispatch_block_t died;

- (void) launch;
- (void) kill;
- (BOOL) isRunning;

@end
