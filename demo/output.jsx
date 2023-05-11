export default (props) => (
  <>
    <let value={false} />
    <div>
      <div class={showClass && "the-class"}>Do stuff after</div>
    </div>
    <WhenVisible
      value={() => {
        console.log(el());
      }}
    />
    <thing
      test={{
        x: 1,
      }}
    ></thing>
  </>
);
