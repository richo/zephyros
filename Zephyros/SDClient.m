//
//  SDClient.m
//  Zephyros
//
//  Created by Steven Degutis on 7/31/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClient.h"

#define FOREVER (60*60*24*365)

#import "SDAPI.h"


@interface SDClient ()

@property int64_t maxRespObjID;
@property NSMutableDictionary* returnedObjects;

@end


@implementation SDClient

- (void) waitForNewMessage {
    [self.sock readDataToData:[@"\n" dataUsingEncoding:NSUTF8StringEncoding]
                  withTimeout:FOREVER
                          tag:0];
}

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag {
    if (tag == 0) {
        NSString* str = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSInteger size = [str integerValue];
        
        [self.sock readDataToLength:size
                        withTimeout:FOREVER
                                tag:1];
    }
    else if (tag == 1) {
        id obj = [NSJSONSerialization JSONObjectWithData:data options:0 error:0];
        [self handleMessage:obj];
        [self waitForNewMessage];
    }
}

- (void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err {
    self.disconnectedHandler(self);
}

- (void) handleMessage:(NSArray*)msg {
    NSNumber* msgID = [msg objectAtIndex:0];
    
    NSNumber* recvID = [msg objectAtIndex:1];
    NSString* meth = [msg objectAtIndex:2];
    NSArray* args = [msg subarrayWithRange:NSMakeRange(3, [msg count] - 3)];
    NSNumber* recv = [self receiverForID:recvID];
    id result = [self callMethod:meth on:recv args:args msgID:msgID];
    
    [self sendResponse:result forID:msgID];
}

- (NSDictionary*) storeObj:(id)obj ofType:(NSString*)type {
    if (!self.returnedObjects)
        self.returnedObjects = [NSMutableDictionary dictionary];
    
    self.maxRespObjID++;
    NSNumber* newMaxID = @(self.maxRespObjID);
    
    [self.returnedObjects setObject:obj
                       forKey:newMaxID];
    
    return @{@"_type": type, @"_id": newMaxID};
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
    else if ([obj isKindOfClass:[SDWindowProxy self]]) {
        return [self storeObj:obj ofType:@"window"];
    }
    else if ([obj isKindOfClass:[SDScreenProxy self]]) {
        return [self storeObj:obj ofType:@"screen"];
    }
    else if ([obj isKindOfClass:[SDAppProxy self]]) {
        return [self storeObj:obj ofType:@"app"];
    }
    
    return obj;
}

- (void) sendResponse:(id)result forID:(NSNumber*)msgID {
    [self sendMessage:@[msgID, [self convertObj:result]]];
//    NSLog(@"%@", self.returnedObjects);
}

- (void) sendMessage:(id)msg {
//    NSLog(@"sending [%@]", msg);
    
    NSData* data = [NSJSONSerialization dataWithJSONObject:msg options:0 error:NULL];
    NSString* len = [NSString stringWithFormat:@"%ld", [data length]];
    [self.sock writeData:[len dataUsingEncoding:NSUTF8StringEncoding] withTimeout:3 tag:0];
    [self.sock writeData:[GCDAsyncSocket LFData] withTimeout:3 tag:0];
    [self.sock writeData:data withTimeout:3 tag:0];
}

- (id) receiverForID:(NSNumber*)recvID {
    if ([recvID integerValue] == 0)
        return nil;
    
    return [self.returnedObjects objectForKey:recvID];
}

- (NSString*) typeForReceiver:(id)recv {
    if (recv == nil) return @"api";
    if ([recv isKindOfClass:[SDWindowProxy self]]) return @"window";
    if ([recv isKindOfClass:[SDScreenProxy self]]) return @"screen";
    if ([recv isKindOfClass:[SDAppProxy self]]) return @"app";
    @throw [NSException exceptionWithName:@"crap" reason:@"uhh" userInfo:nil];
}

+ (NSDictionary*) methods {
    static NSDictionary* methods;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        methods = @{
                    @"api": @{
                            @"focused_window": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                return [SDWindowProxy focusedWindow];
                            },
                            @"choose_from": ^id(SDClient* client, NSNumber* msgID, id recv, NSArray* args) {
                                [SDAPI chooseFrom:[args objectAtIndex:0]
                                            title:[args objectAtIndex:1]
                                            lines:[args objectAtIndex:2]
                                            chars:[args objectAtIndex:3]
                                         callback:^(id idx){
                                             [client sendResponse:idx forID:msgID];
                                         }];
                                return @1;
                            },
                            },
                    @"window": @{
                            @"title": ^id(SDClient* client, NSNumber* msgID, SDWindowProxy* recv, NSArray* args) {
                                return [recv title];
                            },
                            }};
    });
    return methods;
}

- (id) callMethod:(NSString*)meth on:(id)recv args:(NSArray*)args msgID:(NSNumber*)msgID {
//    NSLog(@"recv: %@", recv);
//    NSLog(@"meth: %@", meth);
//    NSLog(@"args: %@", args);
//    NSLog(@"%@", recv);
    
    NSString* type = [self typeForReceiver:recv];
    NSDictionary* methods = [[SDClient methods] objectForKey:type];
    id(^fn)(SDClient* client, NSNumber* msgID, id recv, NSArray* args) = [methods objectForKey:meth];
    
    if (fn)
        return fn(self, msgID, recv, args);
    
    NSLog(@"could not find method [%@] on object of type [%@]", meth, type);
    @throw [NSException exceptionWithName:@"crap" reason:@"uhh" userInfo:nil];
}

@end
