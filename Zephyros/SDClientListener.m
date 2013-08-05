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

#import "SDPreferencesWindowController.h"

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
        self.clients = [NSMutableArray array];
    }
    return self;
}

- (void) startListening {
    self.sock.delegate = nil;
    [self.sock disconnect];
    self.sock = [[GCDAsyncSocket alloc] initWithDelegate:self delegateQueue:dispatch_get_main_queue()];
    
    NSInteger socketType = [[NSUserDefaults standardUserDefaults] integerForKey:SDScriptSocketTypeDefaultsKey];
    if (socketType == 0) {
//        [self.sock acceptOnInterface:@"localhost" port:1235 error:NULL];
    }
    else {
        NSInteger tcpPort = [[NSUserDefaults standardUserDefaults] integerForKey:SDTCPSocketPortDefaultsKey];
        [self.sock acceptOnInterface:@"localhost" port:tcpPort error:NULL];
    }
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
