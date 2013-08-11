//
//  SDAPI.h
//  Zephyros
//
//  Created by Steven on 4/15/13.
//  Copyright (c) 2013 Giant Robot Software. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface SDAPI : NSObject

+ (NSDictionary*) shell:(NSString*)cmd args:(NSArray*)args options:(NSDictionary *)options;

+ (void) chooseFrom:(NSArray*)list
              title:(NSString*)title
              lines:(NSNumber*)linesTall
              chars:(NSNumber*)charsWide
           callback:(void(^)(id idx))callback;

@end
