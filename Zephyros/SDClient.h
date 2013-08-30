//
//  SDClientInterface.h
//  Zephyros
//
//  Created by Steven on 8/11/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "SDRefCache.h"

@protocol SDClientDelegate <NSObject>

- (void) sendResponse:(id)msg;

@end


@interface SDClient : NSObject

- (void) handleRequest:(NSArray*)msg;

- (void) destroy;

@property (weak) id<SDClientDelegate> delegate;

@property NSUndoManager* undoManager;

@property SDRefCache* refCache;


// for client-proxies

- (void) sendResponse:(id)result forID:(NSNumber*)msgID;

@end
