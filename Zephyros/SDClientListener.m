//
//  SDClientListener.m
//  Zephyros
//
//  Created by Steven Degutis on 7/31/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import "SDClientListener.h"

#import "GCDAsyncSocket.h"

#import "SDClient.h"


@interface SDClientListener ()

@property GCDAsyncSocket* sock;
@property NSMutableArray* clients;

@end


@implementation SDClientListener

+ (SDClientListener*) sharedListener {
    static SDClientListener* sharedListener;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedListener = [[SDClientListener alloc] init];
    });
    return sharedListener;
}

- (id) init {
    if (self = [super init]) {
        self.sock = [[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:dispatch_get_main_queue()];
        self.clients = [NSMutableArray array];
    }
    return self;
}

- (void) startListening {
    [self.sock acceptOnInterface:@"localhost" port:1235 error:NULL];
}

- (void)socket:(GCDAsyncSocket *)sock didAcceptNewSocket:(GCDAsyncSocket *)newSocket {
    SDClient* client = [[SDClient alloc] init];
    client.disconnectedHandler = ^(SDClient* me){
        [self.clients removeObject:me];
    };
    newSocket.delegate = client;
    client.sock = newSocket;
    [self.clients addObject:client];
    
    [client waitForNewMessage];
}

@end
