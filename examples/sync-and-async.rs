#![feature(generators)]
#![feature(generator_trait)]

use std::ops::ControlFlow;

use effing_mad::{effectful, handle, handler, run, handle_async};

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(interesting_and_useful());

    boring_and_old_fashioned();
}

// very complex and powerful API
effing_mad::effects! {
    http::HttpRequest {
        fn get(url: &'static str) -> String;
    }
}
use http::HttpRequest;

// this function does not specify whether the request happens synchronously or asynchronously
#[effectful(HttpRequest)]
fn example() -> usize {
    let body = yield HttpRequest::get("http://example.com");
    body.len()
}

async fn interesting_and_useful() {
    let handler = handler! {
        async http::HttpRequest,
        get(url) => {
            let body = reqwest::get(url).await.unwrap().text().await.unwrap();
            ControlFlow::Continue(body)
        },
    };

    let req1 = handle_async(example(), handler);
    let req2 = handle_async(example(), handler);

    // asyncified effectful functions can be composed in the same ways as traditional futures
    let (res1, res2) = futures::future::join(req1, req2).await;
    println!("asynchronously found {res1} and {res2} bytes");
}

fn boring_and_old_fashioned() {
    let handler = handler! {
        http::HttpRequest,
        get(url) => {
            let body = reqwest::blocking::get(url).unwrap().text().unwrap();
            ControlFlow::Continue(body)
        },
    };

    let req = handle(example(), handler);
    let res = run(req);
    println!("synchronously found {res} bytes");
}
