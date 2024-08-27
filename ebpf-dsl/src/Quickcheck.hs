{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quickcheck where


import Data.Proxy
import Hlist
import ListSearch

import Test.QuickCheck


--20 bytes
type Iphdr = Record '[ '("version", Int), --4 bits ip version
                    '("ihl", Int),           --4 bits header length
                    '("tos", Int),           --8 bits type of service
                    '("tot_len", Int),       --16 bits total package length
                    '("id", Int),            --16 bits
                    '("frag_off", Int),      --16 bits
                    '("ttl", Int),           --8 bits
                    '("protocol", Int),      --8 bits Protocol, such as TCP, UDP...
                    '("check", Int),         --16 bits
                    '("saadr", Int),         --32 bits source address
                    '("daddr", Int)]         --32 bits destination address

instance Arbitrary Iphdr where
  arbitrary = do
    version <- elements [4, 6]
    ihl <- choose (5, 15)
    tos <- choose (1, 255)
    tot_len <- choose (20, 65535)
    id_ <- choose (0, 65535)
    frag_off <- choose (0, 8191)
    ttl <- choose (1, 255)
    protocol <- frequency [(50, return 6), (50, choose (1, 255))] --tcp 50% of the time
    check <- arbitrary
    saadr <- arbitrary
    daddr <- arbitrary
    return $ MkPair (Proxy :: Proxy "version") version :#
             MkPair (Proxy :: Proxy "ihl") ihl :#
             MkPair (Proxy :: Proxy "tos") tos :#
             MkPair (Proxy :: Proxy "tot_len") tot_len :#
             MkPair (Proxy :: Proxy "id") id_ :#
             MkPair (Proxy :: Proxy "frag_off") frag_off :#
             MkPair (Proxy :: Proxy "ttl") ttl :#
             MkPair (Proxy :: Proxy "protocol") protocol :#
             MkPair (Proxy :: Proxy "check") check :#
             MkPair (Proxy :: Proxy "saadr") saadr :#
             MkPair (Proxy :: Proxy "daddr") daddr :# HNil

--20 bytes
type Tcphdr = Record '[ '("source", Int), --16 bits
                    '("dest", Int),         --16 bits 
                    '("seq", Int),          --32 bits
                    '("ack_seq", Int),      --32 bits

                    '("resl", Int),         --4 bits
                    '("doff", Int),         --4 bits header length

                    --flags - 1 bit each
                    '("fin", Int),
                    '("syn", Int),
                    '("rst", Int),
                    '("psh", Int),
                    '("ack", Int),
                    '("urg", Int),
                    '("ece", Int),
                    '("cwr", Int),

                    '("window", Int),       --16 bits
                    '("check", Int),        --16 bits
                    '("urg_ptr", Int)]      --16 bits

instance Arbitrary Tcphdr where
  arbitrary = do
    source <- frequency [(50, return 80), (50, choose (0, 65535))] --port 80 50% of the time
    dest <- frequency [(50, return 80), (50, choose (0, 65535))]   --port 80 50% of the time
    seq_ <- choose (0, 4294967295)
    ack_seq <- choose (0, 4294967295)
    resl <- choose (0, 15)
    doff <- choose (5, 15)
    fin <- elements [0, 1]
    syn <- elements [0, 1]
    rst <- elements [0, 1]
    psh <- elements [0, 1]
    ack <- elements [0, 1]
    urg <- elements [0, 1]
    ece <- elements [0, 1]
    cwr <- elements [0, 1]
    window <- choose (0, 65535)
    check <- arbitrary
    urg_ptr <- choose (0, 65535)
    return $ MkPair (Proxy :: Proxy "source") source :#
             MkPair (Proxy :: Proxy "dest") dest :#
             MkPair (Proxy :: Proxy "seq") seq_ :#
             MkPair (Proxy :: Proxy "ack_seq") ack_seq :#
             MkPair (Proxy :: Proxy "resl") resl :#
             MkPair (Proxy :: Proxy "doff") doff :#
             MkPair (Proxy :: Proxy "fin") fin :#
             MkPair (Proxy :: Proxy "syn") syn :#
             MkPair (Proxy :: Proxy "rst") rst :#
             MkPair (Proxy :: Proxy "psh") psh :#
             MkPair (Proxy :: Proxy "ack") ack :#
             MkPair (Proxy :: Proxy "urg") urg :#
             MkPair (Proxy :: Proxy "ece") ece :#
             MkPair (Proxy :: Proxy "cwr") cwr :#
             MkPair (Proxy :: Proxy "window") window :#
             MkPair (Proxy :: Proxy "check") check :#
             MkPair (Proxy :: Proxy "urg_ptr") urg_ptr :# HNil


-- Source e dest são endereços MAC, precisando de 2^48 para serem representados
type Ethhdr = Record '[ '("h_dest", Integer),    --array de 6 palavras de 8 bits = 48 bits
                    '("h_source", Integer),      --array de 6 palavras de 8 bits = 48 bits 
                    '("h_proto", Int)]           --16 bits

instance Arbitrary Ethhdr where
  arbitrary = do
    h_dest <- choose (0, 281474976710655)
    h_source <- choose (0, 281474976710655)
    h_proto <- choose (0, 281474976710655)
    return $ MkPair (Proxy :: Proxy "h_dest") h_dest:#
             MkPair (Proxy :: Proxy "h_source") h_source :#
             MkPair (Proxy :: Proxy "h_proto") h_proto :# HNil


type Xdp_md = Record '[ '("data", Int),      --32 bits
                    '("data_end", Int),         --32 bits
                    '("data_meta", Int),        --32 bits
                    '("ingress_ifindex", Int),  --32 bits
                    '("rx_queue_index", Int),   --32 bits
                    '("egress_ifindex", Int)]   --32 bits

instance Arbitrary Xdp_md where
  arbitrary = do
    data_ <- choose (0, 4294967296)
    data_end <- choose (0, 4294967296)
    data_meta <- choose (0, 4294967296)
    ingress_ifindex <- choose (0, 4294967296)
    rx_queue_index <- choose (0, 4294967296)
    egress_ifindex <- choose (0, 4294967296)
    return $ MkPair (Proxy :: Proxy "data") data_ :#
             MkPair (Proxy :: Proxy "data_end") data_end :#
             MkPair (Proxy :: Proxy "data_meta") data_meta :#
             MkPair (Proxy :: Proxy "ingress_ifindex") ingress_ifindex :#
             MkPair (Proxy :: Proxy "rx_queue_index") rx_queue_index :#
             MkPair (Proxy :: Proxy "egress_ifindex") egress_ifindex :# HNil