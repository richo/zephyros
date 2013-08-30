//
//  SDClientInterface.m
//  Zephyros
//
//  Created by Steven on 8/11/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClient.h"

#import "SDLogWindowController.h"

#import "SDTopLevelRef.h"
#import "SDAppRef.h"
#import "SDWindowRef.h"
#import "SDScreenRef.h"

#import "SDClient.h"

@interface SDClient ()

@property int64_t maxRespObjID;
@property NSMutableDictionary* returnedObjects;

@property SDTopLevelRef* topLevel;

@end


@implementation SDClient

- (id) init {
    if (self = [super init]) {
        self.returnedObjects = [NSMutableDictionary dictionary];
        
        self.topLevel = [[SDTopLevelRef alloc] init];
        self.topLevel.client = self;
        
        self.undoManager = [[NSUndoManager alloc] init];
        
        [self.returnedObjects setObject:self.topLevel forKey:[NSNull null]];
        [self.returnedObjects setObject:self.topLevel forKey:@0]; // backwards compatibility :'(
    }
    return self;
}

- (void) destroy {
    [self.topLevel destroy];
}

- (void) handleRequest:(NSArray*)msg {
    if ([msg count] < 3) {
        SDLogError(@"API error: invalid message: %@", msg);
        return;
    }
    
    id msgID = [msg objectAtIndex:0];
    
    if ([msgID isEqual:[NSNull null]]) {
        SDLogError(@"API error: invalid message id: %@", msgID);
        [self sendResponse:nil forID:msgID];
        return;
    }
    
    id recvID = [msg objectAtIndex:1];
    
    NSString* meth = [msg objectAtIndex:2];
    
    if (![meth isKindOfClass:[NSString self]] || [[meth stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] length] == 0) {
        SDLogError(@"API error: invalid method name: %@", meth);
        [self sendResponse:nil forID:msgID];
        return;
    }
    
    NSArray* args = [msg subarrayWithRange:NSMakeRange(3, [msg count] - 3)];
    SDReference* recv = [self.returnedObjects objectForKey:recvID];
    [recv retainRef];
    [recv releaseRef];
    
    if (recv == nil) {
        SDLogError(@"API Error: Could not find receiver with ID %@", recvID);
        [self sendResponse:nil forID:msgID];
        return;
    }
    
    SEL sel = NSSelectorFromString([[meth stringByReplacingOccurrencesOfString:@"?" withString:@"_q"] stringByAppendingString:@":msgID:"]);
    
    if (![recv respondsToSelector:sel]) {
        SDLogError(@"API Error: Could not find method %@.%@", [recv className], meth);
        [self sendResponse:nil forID:msgID];
        return;
    }
    
    dispatch_async(dispatch_get_main_queue(), ^{
        id result = nil;
        @try {
            #pragma clang diagnostic push
            #pragma clang diagnostic ignored "-Warc-performSelector-leaks"
            result = [recv performSelector:sel withObject:args withObject:msgID];
            #pragma clang diagnostic pop
        }
        @catch (NSException *exception) {
            SDLogError([exception description]);
        }
        @finally {
            [self sendResponse:result forID:msgID];
        }
    });
}

- (NSNumber*) storeObj:(id)obj withWrapper:(Class)wrapper {
    self.maxRespObjID++;
    NSNumber* newMaxID = @(self.maxRespObjID);
    
    SDReference* wrappedObj = [[wrapper alloc] init];
    wrappedObj.client = self;
    wrappedObj.receiver = obj;
    
    [self.returnedObjects setObject:wrappedObj
                             forKey:newMaxID];
    
    __weak SDClient* _self = self;
    
    wrappedObj.whenFinallyDead = ^{
        [_self.returnedObjects removeObjectForKey:newMaxID];
    };
    
    [wrappedObj retainRef];
    [wrappedObj releaseRef];
    
    return newMaxID;
}

- (id) convertObj:(id)obj {
    if (obj == nil) {
        return [NSNull null];
    }
    else if ([obj isKindOfClass:[NSArray self]]) {
        NSMutableArray* newArray = [NSMutableArray array];
        
        for (id child in obj) {
            [newArray addObject:[self convertObj:child]];
        }
        
        return newArray;
    }
    else if ([obj isKindOfClass:[SDWindow self]]) {
        return [self storeObj:obj withWrapper:[SDWindowRef self]];
    }
    else if ([obj isKindOfClass:[NSScreen self]]) {
        return [self storeObj:obj withWrapper:[SDScreenRef self]];
    }
    else if ([obj isKindOfClass:[SDApp self]]) {
        return [self storeObj:obj withWrapper:[SDAppRef self]];
    }
    
    return obj;
}

- (void) sendResponse:(id)result forID:(NSNumber*)msgID {
    [self.delegate sendResponse:@[msgID, [self convertObj:result]]];
}

@end
